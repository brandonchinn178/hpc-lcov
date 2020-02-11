{-# LANGUAGE TupleSections #-}

module Trace.Hpc.Codecov
  ( generateCodecovFromTix
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.List (foldl', foldl1')
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as Text
import Trace.Hpc.Mix (BoxLabel(..), Mix(..), MixEntry)
import Trace.Hpc.Tix (TixModule, tixModuleName, tixModuleTixs)
import Trace.Hpc.Util (HpcPos, fromHpcPos, insideHpcPos)

import Trace.Hpc.Codecov.Report (CodecovReport(..), FileReport(..), Hit(..))

-- | Generate Codecov JSON format from HPC coverage data.
generateCodecovFromTix
  :: [(String, Mix)] -- ^ Mapping from module name to .mix file
  -> [TixModule]
  -> CodecovReport
generateCodecovFromTix moduleToMix = CodecovReport . map mkFileReport
  where
    mkFileReport tixModule =
      let tickCounts = tixModuleTixs tixModule
          moduleName = tixModuleName tixModule
          Mix fileLoc _ _ _ mixEntries =
            fromMaybe
              (error $ "Could not find .mix file for: " ++ moduleName)
              $ moduleName `lookup` moduleToMix
      in FileReport (Text.pack fileLoc) (mkLineHits $ zip mixEntries tickCounts)

type TickCount = Integer

-- | Return a mapping from line count to Hit.
--
-- If a line is resolved with multiple hits, it can mean one of two things:
--   (1) The hits originate from a Box within another Box, in which case, keep the hit from the more
--      specific box
--   (2) The hits originate from disjoint boxes. This can cause one of the following cases:
--      (a) All hits are zero, in which case, the line has a hit of zero (i.e. a MISS)
--      (b) Some hits are zero, in which case, the line has a PARTIAL hit
--      (c) No hits are zero, in which case, keep the maximum hit
mkLineHits :: [(MixEntry, TickCount)] -> IntMap Hit
mkLineHits = fmap resolveHits . concatIntMaps . map (uncurry getLineHits) . filterMixEntries
  where
    -- N.B. IntMap list is guaranteed to be non-empty
    concatIntMaps :: [IntMap v] -> IntMap [v]
    concatIntMaps = IntMap.unionsWith (++) . map (fmap (:[]))

    -- `foldl1'` should not be partial here, since `concatIntMaps` returns a non-empty list and
    -- `filterNonDisjointHits` always returns a non-empty list if it's given a non-empty list
    resolveHits = foldl1' resolveDisjointHits . filterNonDisjointHits

    -- combines the hits for each box on a given line
    resolveDisjointHits (Hit 0) (Hit 0) = Hit 0
    resolveDisjointHits (Hit x) (Hit y) | x /= 0 && y /= 0 = Hit $ max x y
    resolveDisjointHits _ _ = Partial

-- | Get hits per line in the given HpcPos.
getLineHits :: HpcPos -> TickCount -> IntMap (Hit, HpcPos)
getLineHits hpcPos tickCount = IntMap.fromList $ map (, (hit, hpcPos)) [lineStart..lineEnd]
  where
    hit = Hit $ fromInteger tickCount
    (lineStart, _, lineEnd, _) = fromHpcPos hpcPos

-- | Filter out mix entries that should not be included in the coverage report.
filterMixEntries :: [(MixEntry, TickCount)] -> [(HpcPos, TickCount)]
filterMixEntries = mapMaybe $ \((hpcPos, boxLabel), tickCount) ->
  case boxLabel of
    ExpBox _ -> Just (hpcPos, tickCount)

    -- BinBox specifies a box that evaluates to a Bool and counts the number of times the box
    -- evaluates to True/False. We don't care about this information for codecov.
    BinBox _ _ -> Nothing

    -- TopLevelBox/LocalBox specifies boxes counting the number of times a function has been hit.
    -- Codecov doesn't care about this, just use the number of times each expression in the function
    -- was hit.
    TopLevelBox _ -> Nothing
    LocalBox _ -> Nothing

-- see point (1) in mkLineHits
--
-- Returns an empty list iff the input is an empty list
filterNonDisjointHits :: [(Hit, HpcPos)] -> [Hit]
filterNonDisjointHits = map fst . foldl' makeDisjoint []
  where
    makeDisjoint disjointHits hit =
      if any (hit `contains`) disjointHits
        then disjointHits
        else hit : exclude (`contains` hit) disjointHits

    (_, pos1) `contains` (_, pos2) = insideHpcPos pos2 pos1
    exclude f = filter (not . f)
