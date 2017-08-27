{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Data.Monoid
import Data.Text (Text)
import Data.Time (UTCTime)
import Hyperion.Measurement (Sample)
import Hyperion.Internal

-- | Extra metadata provided by the user
newtype UserMetadata = UserMetadata
  { unUserMetadata :: [(Text, Text)]
  } deriving Monoid

data Report = Report
  { _reportBenchName :: !Text
  , _reportBenchParams :: [Int]
  , _reportTimeInNanos :: !Double
  , _reportCycles :: !(Maybe Double)
  , _reportAlloc :: !(Maybe Int64)
  , _reportGarbageCollections :: !(Maybe Int64)
  , _reportMeasurements :: !(Maybe Sample)
  } deriving (Generic)
makeLenses ''Report
deriveJSON defaultOptions{ fieldLabelModifier = camelTo2 '_' . drop (length @[] "_report") } ''Report

json
  :: UTCTime -- ^ Current time
  -> Maybe Text -- ^ Host
  -> UserMetadata -- ^ Extra user metadata
  -> HashMap BenchmarkId Report -- ^ Report to encode
  -> JSON.Value
json timestamp hostId md report =
    JSON.object
      [ "metadata" .= JSON.object (
          -- append metadata at the end so that the user can rewrite
          -- @timestamp@, for instance
          [ "timestamp" .= timestamp, "location" .= hostId ] <>
              ((fmap JSON.toJSON) <$> (unUserMetadata md))
        )
      , "results" .= HashMap.elems report
      ]
