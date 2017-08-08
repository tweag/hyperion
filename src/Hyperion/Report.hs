{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Hyperion.Report where

import Control.Lens.TH (makeLenses)
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import Data.Aeson ((.=))
import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import Data.Text (Text)
import Data.Time (UTCTime)
import Hyperion.Measurement (Sample)

data Report = Report
  { _reportBenchName :: !Text
  , _reportTimeInNanos :: !Double
  , _reportCycles :: !(Maybe Double)
  , _reportAlloc :: !(Maybe Int64)
  , _reportGarbageCollections :: !(Maybe Int64)
  , _reportMeasurements :: !(Maybe Sample)
  } deriving (Generic)
makeLenses ''Report
deriveJSON defaultOptions{ fieldLabelModifier = camelTo2 '_' . drop (length @[] "_report") } ''Report

json :: UTCTime -> Maybe Text -> HashMap Text Report -> JSON.Value
json timestamp hostId report =
    JSON.object
      [ "metadata" .= JSON.object [ "timestamp" .= timestamp, "location" .= hostId ]
      , "results" .= HashMap.elems report
      ]
