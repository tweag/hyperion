{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Hyperion.Report where

import Control.Lens.TH (makeLenses)
import Data.Monoid
import GHC.Generics (Generic)
import qualified Data.Aeson as JSON
import Data.Aeson ((.=))
import Data.Aeson.TH
import Data.Aeson.Types (camelTo2)
import Data.Bifunctor (first)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Int
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector as V
import Hyperion.Measurement (Sample)
import Hyperion.Internal

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
  :: JSON.Object -- ^ Metadata
  -> HashMap BenchmarkId Report -- ^ Report to encode
  -> JSON.Value
json md report =
    JSON.object
      [ "metadata" .= md
      , "results" .= HashMap.elems report
      ]

jsonFlat
  :: JSON.Object -- ^ Metadata
  -> HashMap BenchmarkId Report
  -- ^ Report to encode
  -> JSON.Value
jsonFlat md report = jsonList $ (`map` HashMap.elems report) $ \b ->
    JSON.object $
      HashMap.toList md <> (flatten $ JSON.toJSON b)
  where
    flatten (JSON.Object b) = concatMap splitParams $ HashMap.toList b
    flatten x = [("result", x)] -- XXX should never happen
    jsonList = JSON.Array . V.fromList
    -- flatten the benchmark params
    splitParams ("bench_params", JSON.Array xs) =
      fmap (first (mappend "x_" . Text.pack . show)) (zip [(1::Int)..] $ V.toList xs)
    splitParams x = [x]
