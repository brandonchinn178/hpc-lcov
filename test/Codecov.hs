{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Codecov where

import Control.Arrow ((&&&))
import Data.Aeson (encode)
import qualified Data.IntMap as IntMap
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
    pure $ encode $ generateCodecovFromTix $ map mkTixMix
      [ TixMix "src/MyModule/Foo.hs"
          [ TixMixEntry (1, 1) (1, 20) (TopLevelBox ["foo"]) 0
          , TixMixEntry (2, 1) (2, 20) (ExpBox True) 1
          , TixMixEntry (3, 1) (3, 20) (ExpBox False) 2
          ]
      , TixMix "src/MyModule/Bar.hs"
          [ TixMixEntry (1, 1) (1, 20) (TopLevelBox ["bar"]) 10
          ]
      , TixMix "src/MyModule/Bar/Baz.hs"
          [ TixMixEntry (1, 1) (1, 20) (TopLevelBox ["baz"]) 20
          ]
      ]

test_generate_codecov_merge_hits :: TestTree
test_generate_codecov_merge_hits = testCase "generateCodecovFromTix merge hits" $
  let report = generateCodecovFromTix $ map mkTixMix
        [ TixMix "WithPartial.hs"
            [ TixMixEntry (1, 1) (1, 5) (ExpBox True) 10
            , TixMixEntry (1, 10) (1, 20) (ExpBox False) 0
            ]
        , TixMix "WithMissing.hs"
            [ TixMixEntry (1, 1) (1, 5) (ExpBox True) 0
            , TixMixEntry (1, 10) (1, 20) (ExpBox False) 0
            ]
        , TixMix "WithMax.hs"
            [ TixMixEntry (1, 1) (1, 5) (ExpBox True) 10
            , TixMixEntry (1, 10) (1, 20) (ExpBox False) 20
            ]
        ]
  in fromReport report @?=
    [ ("WithPartial.hs", [(1, Partial)])
    , ("WithMissing.hs", [(1, Hit 0)])
    , ("WithMax.hs", [(1, Hit 20)])
    ]

test_generate_codecov_binbox :: TestTree
test_generate_codecov_binbox = testCase "generateCodecovFromTix BinBox" $
  let report = generateCodecovFromTix $ map mkTixMix
        [ TixMix "WithBinBox.hs"
            -- if [x > 0] then ... else ...
            --    ^ evaluates to True 10 times, False 0 times
            --    | should show in the report as "10 hits"
            [ TixMixEntry (1, 1) (1, 10) (ExpBox True) 10
            , TixMixEntry (1, 1) (1, 10) (BinBox CondBinBox True) 10
            , TixMixEntry (1, 1) (1, 10) (BinBox CondBinBox False) 0
            ]
        ]
  in fromReport report @?=
    [ ("WithBinBox.hs", [(1, Hit 10)])
    ]

{- Helpers -}

mkTix :: [Integer] -> TixModule
mkTix ticks = TixModule "A.B.C" 0 (length ticks) ticks

data MixEntry = MixEntry
  { mixEntryStartPos :: (Int, Int)
  , mixEntryEndPos   :: (Int, Int)
  , mixEntryBoxLabel :: BoxLabel
  }

mkMix :: FilePath -> [MixEntry] -> Mix
mkMix filePath mixEntries = Mix filePath updateTime 0 (length mixs) mixs
  where
    updateTime = UTCTime (fromGregorian 1970 1 1) 0
    mixs = flip map mixEntries $ \MixEntry{..} ->
      let (startLine, startCol) = mixEntryStartPos
          (endLine, endCol) = mixEntryEndPos
      in (toHpcPos (startLine, startCol, endLine, endCol), mixEntryBoxLabel)

data TixMix = TixMix
  { tixMixFilePath :: FilePath
  , tixMixEntries  :: [TixMixEntry]
  }

data TixMixEntry = TixMixEntry
  { tixMixEntryStartPos :: (Int, Int)
  , tixMixEntryEndPos   :: (Int, Int)
  , tixMixEntryBoxLabel :: BoxLabel
  , tixMixEntryTicks    :: Integer
  }

mkTixMix :: TixMix -> (TixModule, Mix)
mkTixMix TixMix{..} = (mkTix ticks, mkMix tixMixFilePath mixEntries)
  where
    ticks = map tixMixEntryTicks tixMixEntries
    mixEntries = flip map tixMixEntries $
      \(TixMixEntry start end boxLabel _) -> MixEntry start end boxLabel

fromReport :: CodecovReport -> [(Text, [(Int, Hit)])]
fromReport (CodecovReport fileReports) = map (fileName &&& getHits) fileReports
  where
    getHits = IntMap.toList . lineHits
