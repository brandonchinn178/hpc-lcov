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
      [ TixMix "MyModule.Foo" "src/MyModule/Foo.hs"
          [ TixEntry (1, 1) (1, 20) (TopLevelBox ["foo"]) 0
          , TixEntry (2, 1) (2, 20) (ExpBox True) 1
          , TixEntry (3, 1) (3, 20) (ExpBox False) 2
          ]
      , TixMix "MyModule.Bar" "src/MyModule/Bar.hs"
          [ TixEntry (1, 1) (1, 20) (TopLevelBox ["bar"]) 10
          ]
      , TixMix "MyModule.Bar.Baz" "src/MyModule/Bar/Baz.hs"
          [ TixEntry (1, 1) (1, 20) (TopLevelBox ["baz"]) 20
          ]
      ]

test_generate_codecov_merge_hits :: TestTree
test_generate_codecov_merge_hits = testCase "generateCodecovFromTix merge hits" $
  let report = generateCodecovFromTix $ map mkTixMix
        [ TixMix "WithPartial" "WithPartial.hs"
            [ TixEntry (1, 1) (1, 5) (ExpBox True) 10
            , TixEntry (1, 10) (1, 20) (ExpBox False) 0
            ]
        , TixMix "WithMissing" "WithMissing.hs"
            [ TixEntry (1, 1) (1, 5) (ExpBox True) 0
            , TixEntry (1, 10) (1, 20) (ExpBox False) 0
            ]
        , TixMix "WithMax" "WithMax.hs"
            [ TixEntry (1, 1) (1, 5) (ExpBox True) 10
            , TixEntry (1, 10) (1, 20) (ExpBox False) 20
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
        [ TixMix "WithBinBox" "WithBinBox.hs"
            -- if [x > 0] then ... else ...
            --    ^ evaluates to True 10 times, False 0 times
            --    | should show in the report as "10 hits"
            [ TixEntry (1, 1) (1, 10) (ExpBox True) 10
            , TixEntry (1, 1) (1, 10) (BinBox CondBinBox True) 10
            , TixEntry (1, 1) (1, 10) (BinBox CondBinBox False) 0
            ]
        ]
  in fromReport report @?=
    [ ("WithBinBox.hs", [(1, Hit 10)])
    ]

{- Helpers -}

data TixMix = TixMix
  { tixMixModule   :: String
  , tixMixFilePath :: FilePath
  , tixMixEntries  :: [TixEntry]
  }

data TixEntry = TixEntry
  { tixEntryStartPos :: (Int, Int)
  , tixEntryEndPos   :: (Int, Int)
  , tixEntryBoxLabel :: BoxLabel
  , tixEntryTicks    :: Integer
  }

mkTixMix :: TixMix -> (TixModule, Mix)
mkTixMix TixMix{..} = (tixModule, mix)
  where
    tixModule = TixModule tixMixModule 0 (length ticks) ticks
    mix = Mix tixMixFilePath updateTime 0 (length mixEntries) mixEntries

    updateTime = UTCTime (fromGregorian 1970 1 1) 0
    ticks = map tixEntryTicks tixMixEntries
    mixEntries = flip map tixMixEntries $ \TixEntry{..} ->
      let (startLine, startCol) = tixEntryStartPos
          (endLine, endCol) = tixEntryEndPos
      in (toHpcPos (startLine, startCol, endLine, endCol), tixEntryBoxLabel)

fromReport :: CodecovReport -> [(Text, [(Int, Hit)])]
fromReport (CodecovReport fileReports) = map (fileName &&& getHits) fileReports
  where
    getHits = IntMap.toList . lineHits
