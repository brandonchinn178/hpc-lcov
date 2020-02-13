{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Trace.Hpc.Lcov.Report
  ( CodecovReport(..)
  , FileReport(..)
  , Hit(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Text (Text)
import qualified Data.Text as Text

-- https://docs.codecov.io/reference#section-codecov-json-report-format
newtype CodecovReport = CodecovReport [FileReport]
  deriving (Show, Eq)

instance ToJSON CodecovReport where
  toJSON (CodecovReport fileReports) = object
    [ "coverage" .= objectWith fromReport fileReports
    ]
    where
      objectWith f = object . map f
      fromReport FileReport{..} = fileName .= objectWith fromHit (IntMap.toList lineHits)
      fromHit (lineNum, hit) = Text.pack (show lineNum) .= hit

data FileReport = FileReport
  { fileName :: Text
  , lineHits :: IntMap Hit
  } deriving (Show, Eq)

data Hit
  = Hit Int -- should never be negative
  | Partial
  deriving (Show, Eq)

instance ToJSON Hit where
  toJSON = \case
    Hit count -> toJSON count
    Partial -> toJSON ("1/2" :: String)
