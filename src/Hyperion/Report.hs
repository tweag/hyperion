{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.Function (on)
import Data.Hashable (Hashable(..))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import Data.Monoid
import qualified Data.Text as Text
import Data.Text (Text)
import Data.Time (UTCTime)
import Hyperion.Measurement (Sample)

data Parameter = forall a. (Show a, Enum a) => Parameter a

data Component
  = BenchC Text
  | GroupC Text
  | SeriesC Parameter

newtype BenchmarkId = BenchmarkId [Component]

instance Eq BenchmarkId where
  (==) = (==) `on` renderBenchmarkId

instance Ord BenchmarkId where
  compare = compare `on` renderBenchmarkId

instance Hashable BenchmarkId where
  hashWithSalt s = hashWithSalt s . renderBenchmarkId

instance Show BenchmarkId where
  show = Text.unpack . renderBenchmarkId

renderBenchmarkId :: BenchmarkId -> Text
renderBenchmarkId (BenchmarkId comps0) = go "" comps0
  where
    go index [BenchC txt] = txt <> index
    go index (GroupC txt : comps) = txt <> index <> "/" <> go "" comps
    go index (SeriesC (Parameter x) : comps) =
        go (index <> ":" <> Text.pack (show x)) comps
    go _ _ = error "renderBenchmarkId: Impossible"

benchmarkParameters :: BenchmarkId -> [Parameter]
benchmarkParameters (BenchmarkId comps) =
    concatMap (\case SeriesC x -> [x]; _ -> []) comps

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

json :: UTCTime -> Maybe Text -> HashMap BenchmarkId Report -> JSON.Value
json timestamp hostId report =
    JSON.object
      [ "metadata" .= JSON.object [ "timestamp" .= timestamp, "location" .= hostId ]
      , "results" .= HashMap.elems report
      ]
