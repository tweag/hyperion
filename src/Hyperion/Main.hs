{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE RecordWildCards #-}

module Hyperion.Main
  ( defaultMain
  , Config
  , defaultConfig
  , defaultMainWith
  ) where

import Control.Applicative
import Control.Lens ((&), (.~), (%~), (%@~), (^..), folded, imapped, mapped)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Time (getCurrentTime)
import Data.Version (showVersion)
import Hyperion.Analysis
import Hyperion.Benchmark
import Hyperion.Measurement
import Hyperion.Report
import Hyperion.Run
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Aeson as JSON
import qualified Options.Applicative as Options
import Paths_hyperion (version)

data Mode = Version | List | Run | Analyze
  deriving (Eq, Ord, Show)

data ConfigMonoid = ConfigMonoid
  { configMonoidMode :: First Mode
  , configMonoidRaw :: First Bool
  }

instance Monoid ConfigMonoid where
  mempty =
    ConfigMonoid
      mempty
      mempty
  mappend c1 c2 =
    ConfigMonoid
      (mappend (configMonoidMode c1) (configMonoidMode c2))
      (mappend (configMonoidRaw c1) (configMonoidRaw c2))

data Config = Config
  { configMode :: Mode
  , configRaw :: Bool
  }

fromFirst :: a -> First a -> a
fromFirst x = fromMaybe x . getFirst

configFromMonoid :: ConfigMonoid -> Config
configFromMonoid ConfigMonoid{..} = Config
    { configMode = fromFirst Analyze configMonoidMode
    , configRaw = fromFirst False configMonoidRaw
    }

options :: Options.Parser ConfigMonoid
options = do
     configMonoidMode <-
       First <$> optional
         (Options.flag' Version
            (Options.long "version" <>
             Options.hidden <>
             Options.help "Display version information") <|>
          Options.flag' List
            (Options.long "list" <>
             Options.short 'l' <>
             Options.help "List benchmark names") <|>
          Options.flag' Analyze
            (Options.long "run" <>
             Options.help "Run benchmarks and analyze them (default)") <|>
          Options.flag' Run
            (Options.long "no-analyze" <>
             Options.help "Only run the benchmarks"))
     configMonoidRaw <-
       First <$> optional
         (Options.switch
            (Options.long "raw" <>
             Options.help "Include raw measurement data in report."))
     pure ConfigMonoid{..}

defaultConfig :: ConfigMonoid
defaultConfig = mempty

doList :: [Benchmark] -> IO ()
doList bks = mapM_ Text.putStrLn $ bks^..folded.namesOf

doRun :: [Benchmark] -> IO (HashMap Text Sample)
doRun bks = foldMap (runBenchmark (sample 100 (fixed 5))) bks

doAnalyze :: Config -> [Benchmark] -> IO ()
doAnalyze Config{..} bks = do
    results <- doRun bks
    let strip
          | configRaw = id
          | otherwise = reportMeasurements .~ Nothing
        report = results & imapped %@~ analyze & mapped %~ strip
    now <- getCurrentTime
    BS.putStrLn $ JSON.encode $ json now Nothing report

defaultMainWith :: ConfigMonoid -> [Benchmark] -> IO ()
defaultMainWith presetConfig bks = do
    cmdlineConfig <-
      Options.execParser
        (Options.info
          (Options.helper <*> options)
          Options.fullDesc)
    let config = configFromMonoid (cmdlineConfig <> presetConfig)
    case config of
      Config{..} -> case configMode of
        Version -> putStrLn $ "Hyperion " <> showVersion version
        List -> doList bks
        Run -> do _ <- doRun bks; return ()
        Analyze -> doAnalyze config bks

defaultMain :: [Benchmark] -> IO ()
defaultMain = defaultMainWith defaultConfig
