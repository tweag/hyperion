module Hyperion.Main
  ( defaultMain
  , Config
  , defaultConfig
  , defaultMainWith
  ) where

import Hyperion.Analysis
import Hyperion.Benchmark
import Hyperion.Run
import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as JSON

data Config = Config

defaultConfig :: Config
defaultConfig = Config

data Mode = Version | List | Run | Analyze

defaultMainWith :: Config -> [Benchmark] -> IO ()
defaultMainWith Config bks = do
    results <- foldMap (runBenchmark (fixed 100)) bks
    let report = analyze <$> results
    BS.putStrLn (JSON.encode report)

defaultMain :: [Benchmark] -> IO ()
defaultMain = defaultMainWith defaultConfig
