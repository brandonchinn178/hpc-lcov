{-# LANGUAGE TupleSections #-}

module Trace.Hpc.Codecov
  ( generateCodecovFromTix
  ) where

import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as Text
import Trace.Hpc.Mix (BoxLabel(..), Mix(..), MixEntry)
import Trace.Hpc.Tix (TixModule, tixModuleName, tixModuleTixs)
import Trace.Hpc.Util (fromHpcPos)

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

type LineNum = Int
type TickCount = Integer

mkLineHits :: [(MixEntry, TickCount)] -> IntMap Hit
mkLineHits = IntMap.fromListWith resolveHits . concatMap (uncurry applyTickCountToLines)
  where
    -- combines the hits for each box on a given line
    resolveHits (Hit 0) (Hit 0) = Hit 0
    resolveHits (Hit x) (Hit y) | x /= 0 && y /= 0 = Hit $ max x y
    resolveHits _ _ = Partial

-- | For every line in the given MixEntry, pair it with the number of hits this MixEntry got.
applyTickCountToLines :: MixEntry -> TickCount -> [(LineNum, Hit)]
applyTickCountToLines (hpcPos, boxLabel) tickCount = map (, hit) boxLines
  where
    hit = Hit $ fromInteger tickCount
    (lineStart, _, lineEnd, _) = fromHpcPos hpcPos
    boxLines = case boxLabel of
      -- BinBox specifies a box that evaluates to a Bool and counts the number of times the box
      -- evaluates to True/False. We don't care about this information for codecov.
      BinBox _ _ -> []
      _ -> [lineStart..lineEnd]
