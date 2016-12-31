{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Hyperion.Report where

import Control.Lens.TH (makeLenses)
import GHC.Generics (Generic)
import Data.Aeson.TH
import Data.Int

data Report = Report
  { _reportTime :: Double
  , _reportCycles :: Maybe Double
  , _reportAlloc :: Maybe Int64
  , _reportGCs :: Maybe Int64
  } deriving (Generic)
makeLenses ''Report
deriveJSON defaultOptions{fieldLabelModifier = drop 1} ''Report
