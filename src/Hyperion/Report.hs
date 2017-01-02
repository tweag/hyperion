{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hyperion.Report where

import Control.Lens.TH (makeLenses)
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import Data.Aeson ((.=))
import Data.Aeson.TH
import Data.HashMap.Strict (HashMap)
import Data.Int
import Data.Text (Text)
import Data.Time (UTCTime)

data Report = Report
  { _reportTime :: Double
  , _reportCycles :: Maybe Double
  , _reportAlloc :: Maybe Int64
  , _reportGarbageCollections :: Maybe Int64
  } deriving (Generic)
makeLenses ''Report
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Report

json :: UTCTime -> Maybe Text -> HashMap Text Report -> JSON.Value
json timestamp hostId report =
    JSON.object
      [ "metadata" .= JSON.object [ "timestamp" .= timestamp, "location" .= hostId ]
      , "results" .= report
      ]
