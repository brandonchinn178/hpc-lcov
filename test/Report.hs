{-# LANGUAGE OverloadedStrings #-}

module Report where

import Data.Aeson (encode)
import qualified Data.IntMap as IntMap
import Test.Tasty (TestTree)
import Test.Tasty.Golden (goldenVsString)

import Trace.Hpc.Codecov.Report

test_report_serialization :: TestTree
test_report_serialization =
  goldenVsString "report serialization" "test/golden/report_serialization.golden" $ pure $ encode $
    CodecovReport
      [ FileReport "src/MyModule/Foo.hs" $ IntMap.fromList
          [ (1, Hit 0)
          , (2, Hit 1)
          , (3, Partial)
          , (4, Partial)
          , (7, Hit 5)
          ]
      , FileReport "src/MyModule/Bar.hs" $ IntMap.fromList
          [ (10, Hit 50)
          , (20, Partial)
          ]
      ]
