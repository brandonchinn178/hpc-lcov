{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codecov where

import Control.Arrow ((&&&))
import Data.Aeson (encode)
import qualified Data.IntMap as IntMap
import Data.List (sortOn)
import Data.Text (Text)
import Data.Time (UTCTime(..), fromGregorian)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import Test.Tasty.HUnit (testCase, (@?=))
import Trace.Hpc.Mix (BoxLabel(..), CondBox(..), Mix(..))
import Trace.Hpc.Tix (TixModule(..))
import Trace.Hpc.Util (toHpcPos)

import Trace.Hpc.Codecov
import Trace.Hpc.Codecov.Report (CodecovReport(..), FileReport(..), Hit(..))

test_generate_codecov :: TestTree
test_generate_codecov =
  goldenVsString "generateCodecovFromTix" "test/golden/generate_codecov.golden" $
    pure $ encode $ generateCodecovFromTixMix
      [ TixMix "MyModule.Foo" "src/MyModule/Foo.hs"
          [ TixMixEntry (1, 1) (1, 20) 0
          , TixMixEntry (2, 1) (2, 20) 1
          , TixMixEntry (3, 1) (3, 20) 2
          ]
      , TixMix "MyModule.Bar" "src/MyModule/Bar.hs"
          [ TixMixEntry (1, 1) (1, 20) 10
          ]
      , TixMix "MyModule.Bar.Baz" "src/MyModule/Bar/Baz.hs"
          [ TixMixEntry (1, 1) (1, 20) 20
          ]
      ]

test_generate_codecov_resolve_hits :: TestTree
test_generate_codecov_resolve_hits = testCase "generateCodecovFromTix resolve hits" $
  let report = generateCodecovFromTixMix
        [ TixMix "WithPartial" "WithPartial.hs"
            [ TixMixEntry (1, 1) (1, 5) 10
            , TixMixEntry (1, 10) (1, 20) 0
            ]
        , TixMix "WithMissing" "WithMissing.hs"
            [ TixMixEntry (1, 1) (1, 5) 0
            , TixMixEntry (1, 10) (1, 20) 0
            ]
        , TixMix "WithDisjoint" "WithDisjoint.hs"
            [ TixMixEntry (1, 1) (1, 5) 10
            , TixMixEntry (1, 10) (1, 20) 20
            ]
        , TixMix "WithNonDisjoint" "WithNonDisjoint.hs"
            [ TixMixEntry (1, 1) (5, 20) 20 -- contains all below
            , TixMixEntry (1, 1) (1, 5) 10
            , TixMixEntry (3, 1) (3, 5) 0
            , TixMixEntry (4, 1) (4, 5) 20
            , TixMixEntry (4, 6) (4, 20) 0
            , TixMixEntry (5, 1) (5, 5) 10
            , TixMixEntry (5, 6) (5, 10) 20
            , TixMixEntry (5, 1) (5, 20) 10 -- contains the two above
            ]
        ]
  in fromReport report @?=
    [ ("WithDisjoint.hs", [(1, Hit 20)])
    , ("WithMissing.hs", [(1, Hit 0)])
    , ("WithNonDisjoint.hs", [(1, Hit 10), (2, Hit 20), (3, Hit 0), (4, Partial), (5, Hit 20)])
    , ("WithPartial.hs", [(1, Partial)])
    ]

test_generate_codecov_non_expbox :: TestTree
test_generate_codecov_non_expbox = testCase "generateCodecovFromTix non-ExpBox" $
  let report = generateCodecovFromTix
        [ mkModuleToMix "WithBinBox" "WithBinBox.hs"
            -- if [x > 0] then ... else ...
            --    ^ evaluates to True 10 times, False 0 times
            --      should show in the report as "10 hits", not
            --      as "partial"
            [ MixEntry (1, 1) (1, 10) (ExpBox True)
            , MixEntry (1, 1) (1, 10) (BinBox CondBinBox True)
            , MixEntry (1, 1) (1, 10) (BinBox CondBinBox False)
            ]
        , mkModuleToMix "WithTopLevelBox" "WithTopLevelBox.hs"
            -- foo x = ...
            -- ^
            [ MixEntry (1, 1) (2, 10) (TopLevelBox ["foo"])
            , MixEntry (1, 1) (1, 5) (ExpBox True)
            , MixEntry (2, 6) (2, 10) (ExpBox True)
            ]
        , mkModuleToMix "WithLocalBox" "WithLocalBox.hs"
            -- foo x = ...
            --   where bar = ...
            --         ^
            [ MixEntry (1, 1) (1, 10) (LocalBox ["foo", "bar"])
            , MixEntry (1, 1) (1, 5) (ExpBox True)
            , MixEntry (1, 6) (1, 10) (ExpBox True)
            ]
        ]
        [ mkTix "WithBinBox" [10, 10, 0]
        , mkTix "WithTopLevelBox" [1, 0, 1]
        , mkTix "WithLocalBox" [1, 1, 1]
        ]
  in fromReport report @?=
    [ ("WithBinBox.hs", [(1, Hit 10)])
    , ("WithLocalBox.hs", [(1, Hit 1)])
    , ("WithTopLevelBox.hs", [(1, Hit 0), (2, Hit 1)])
    ]

{- Helpers -}

mkTix :: String -> [Integer] -> TixModule
mkTix moduleName ticks = TixModule moduleName 0 (length ticks) ticks

data MixEntry = MixEntry
  { mixEntryStartPos :: (Int, Int)
  , mixEntryEndPos   :: (Int, Int)
  , mixEntryBoxLabel :: BoxLabel
  }

mkModuleToMix :: String -> FilePath -> [MixEntry] -> (String, Mix)
mkModuleToMix moduleName filePath mixEntries = (moduleName, Mix filePath updateTime 0 (length mixs) mixs)
  where
    updateTime = UTCTime (fromGregorian 1970 1 1) 0
    mixs = flip map mixEntries $ \MixEntry{..} ->
      let (startLine, startCol) = mixEntryStartPos
          (endLine, endCol) = mixEntryEndPos
      in (toHpcPos (startLine, startCol, endLine, endCol), mixEntryBoxLabel)

data TixMix = TixMix
  { tixMixModule   :: String
  , tixMixFilePath :: FilePath
  , tixMixEntries  :: [TixMixEntry]
  }

data TixMixEntry = TixMixEntry
  { tixMixEntryStartPos :: (Int, Int)
  , tixMixEntryEndPos   :: (Int, Int)
  , tixMixEntryTicks    :: Integer
  }

generateCodecovFromTixMix :: [TixMix] -> CodecovReport
generateCodecovFromTixMix = uncurry generateCodecovFromTix . unzip . map fromTixMix
  where
    fromTixMix TixMix{..} =
      let toMixEntry (TixMixEntry start end _) = MixEntry start end (ExpBox True)
          moduleToMix = mkModuleToMix tixMixModule tixMixFilePath $ map toMixEntry tixMixEntries
          tix = mkTix tixMixModule $ map tixMixEntryTicks tixMixEntries
      in (moduleToMix, tix)

fromReport :: CodecovReport -> [(Text, [(Int, Hit)])]
fromReport (CodecovReport fileReports) = sortOn fst $ map (fileName &&& getHits) fileReports
  where
    getHits = IntMap.toList . lineHits
