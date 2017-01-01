{-# LANGUAGE RecordWildCards #-}

module Hyperion.Main
  ( defaultMain
  , Config
  , defaultConfig
  , defaultMainWith
  ) where

import Control.Applicative
import Control.Lens ((^..), folded)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Version (showVersion)
import Hyperion.Analysis
import Hyperion.Benchmark
import Hyperion.Measurement
import Hyperion.Run
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Aeson as JSON
import qualified Options.Applicative as Options
import Paths_hyperion (version)

data Mode = Version | List | Run | Analyze
  deriving (Eq, Ord, Show)

data ConfigMonoid = ConfigMonoid
  { configMonoidMode :: First Mode
  }

instance Monoid ConfigMonoid where
  mempty = ConfigMonoid mempty
  mappend c1 c2 = ConfigMonoid (mappend (configMonoidMode c1) (configMonoidMode c2))

data Config = Config
  { configMode :: Mode
  }

fromFirst :: a -> First a -> a
fromFirst x = fromMaybe x . getFirst

configFromMonoid :: ConfigMonoid -> Config
configFromMonoid ConfigMonoid{..} = Config
    { configMode = fromFirst Analyze configMonoidMode }

options :: Options.Parser ConfigMonoid
options =
    ConfigMonoid <$>
      (First <$> optional
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
            Options.help "Only run the benchmarks")))

defaultConfig :: ConfigMonoid
defaultConfig = mempty

doList :: [Benchmark] -> IO ()
doList bks = mapM_ Text.putStrLn $ bks^..folded.namesOf

doRun :: [Benchmark] -> IO (HashMap Text Sample)
doRun bks = foldMap (runBenchmark (fixed 100)) bks

doAnalyze :: [Benchmark] -> IO ()
doAnalyze bks = do
    results <- doRun bks
    let report = analyze <$> results
    BS.putStrLn (JSON.encode report)

defaultMainWith :: ConfigMonoid -> [Benchmark] -> IO ()
defaultMainWith config bks = do
    cmdlineConfig <- Options.execParser (Options.info (Options.helper <*> options) (Options.fullDesc))
    case configFromMonoid (cmdlineConfig <> config) of
      Config{..} -> case configMode of
        Version -> putStrLn $ "Hyperion " <> showVersion version
        List -> doList bks
        Run -> do _ <- doRun bks; return ()
        Analyze -> doAnalyze bks

defaultMain :: [Benchmark] -> IO ()
defaultMain = defaultMainWith defaultConfig
