{-# LANGUAGE RecordWildCards #-}

module Codecov where

import Data.Aeson (encode)
import Data.Time (UTCTime(..), fromGregorian)
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)
import Trace.Hpc.Mix (BoxLabel(..), Mix(..))
import Trace.Hpc.Tix (TixModule(..))
import Trace.Hpc.Util (toHpcPos)

import Trace.Hpc.Codecov

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
