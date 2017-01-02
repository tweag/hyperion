{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Hyperion.Report where

import Control.Lens.TH (makeLenses)
import GHC.Generics (Generic)
import Data.Aeson.TH
import Data.Int

data Report = Report
  { _reportTimeMean :: Maybe Double
  , _reportTimeMedian :: Maybe Double
  , _reportTimeMax :: Maybe Double
  , _reportTime90 :: Maybe Double
  , _reportTime99 :: Maybe Double
  , _reportTime99_9 :: Maybe Double
  , _reportCycles :: Maybe Double
  , _reportAlloc :: Maybe Int64
  , _reportGCs :: Maybe Int64
  } deriving (Generic)
makeLenses ''Report
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Report
