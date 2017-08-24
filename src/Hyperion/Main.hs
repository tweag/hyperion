{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hyperion.Main
  ( defaultMain
  , Mode(..)
  , ConfigMonoid(..)
  , nullOutputPath
  , defaultConfig
  , defaultMainWith
  ) where

import Control.Applicative
import Control.Exception (Exception, throwIO)
import Control.Lens ((&), (.~), (%~), (%@~), (^..), folded, imapped, mapped, to)
import Control.Monad (unless, when, mzero)
import Data.HashMap.Strict (HashMap)
import Data.List (group, sort)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (pack, Text, unpack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (getCurrentTime)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Hyperion.Analysis
import Hyperion.Benchmark
import Hyperion.Internal
import Hyperion.Measurement
import Hyperion.PrintReport
import Hyperion.Report
import Hyperion.Run
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Aeson as JSON
import qualified Options.Applicative as Options
import Paths_hyperion (version)
import qualified System.IO as IO
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (getProgName)
import System.FilePath ((</>), (<.>))
import System.FilePath.Posix (hasTrailingPathSeparator)

data Mode = Version | List | Run | Analyze
  deriving (Eq, Ord, Show)

data ConfigMonoid = ConfigMonoid
  { configMonoidOutputPath :: First FilePath
  , configMonoidMode :: First Mode
  , configMonoidPretty :: First Bool
  , configMonoidRaw :: First Bool
  , configMonoidSamplingStrategy :: First SamplingStrategy
  , configMonoidExtraMetadata :: UserMetadata
  }

instance Monoid ConfigMonoid where
  mempty =
    ConfigMonoid
      mempty
      mempty
      mempty
      mempty
      mempty
      mempty
  mappend c1 c2 =
    ConfigMonoid
      (mappend (configMonoidOutputPath c1) (configMonoidOutputPath c2))
      (mappend (configMonoidMode c1) (configMonoidMode c2))
      (mappend (configMonoidPretty c1) (configMonoidPretty c2))
      (mappend (configMonoidRaw c1) (configMonoidRaw c2))
      (mappend (configMonoidSamplingStrategy c1) (configMonoidSamplingStrategy c2))
      (mappend (configMonoidExtraMetadata c1) (configMonoidExtraMetadata c2))

data Config = Config
  { configOutputPath :: Maybe FilePath
  , configMode :: Mode
  , configPretty :: Bool
  , configRaw :: Bool
  , configSamplingStrategy :: SamplingStrategy
  , configExtraMetadata :: UserMetadata
  }

fromFirst :: a -> First a -> a
fromFirst x = fromMaybe x . getFirst

configFromMonoid :: ConfigMonoid -> Config
configFromMonoid ConfigMonoid{..} = Config
    { configOutputPath = getFirst configMonoidOutputPath
    , configMode = fromFirst Analyze configMonoidMode
    , configPretty = fromFirst False configMonoidPretty
    , configRaw = fromFirst False configMonoidRaw
    , configSamplingStrategy = fromFirst defaultStrategy configMonoidSamplingStrategy
    , configExtraMetadata = configMonoidExtraMetadata
    }

options :: Options.Parser ConfigMonoid
options = do
     configMonoidOutputPath <-
       First <$> optional
         (Options.strOption
            (Options.long "output" <>
             Options.short 'o' <>
             Options.help "Where to write the benchmarks output. Can be a directory name" <>
             Options.metavar "PATH"))
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
     configMonoidPretty <-
       First <$> optional
         (Options.switch
            (Options.long "pretty" <>
             Options.help "Pretty prints the measurements on stdout."))
     configMonoidRaw <-
       First <$> optional
         (Options.switch
            (Options.long "raw" <>
             Options.help "Include raw measurement data in report."))
     configMonoidExtraMetadata <-
       many
         (Options.option
            toTup
            (Options.long "arg" <>
             Options.help "Include raw measurement data in report."))
     -- TODO allow setting this from CLI.
     pure ConfigMonoid{..}
  where
    configMonoidSamplingStrategy = First Nothing
    toTup = do
      txt <- Text.pack <$> Options.str
      case Text.splitOn ":" txt of
        [x,y] -> pure (x,y)
        _ -> mzero

-- | The path to the null output file. This is @"nul"@ on Windows and
-- @"/dev/null"@ elsewhere.
nullOutputPath :: FilePath
#ifdef mingw32_HOST_OS
nullOutputPath = "nul"
#else
nullOutputPath = "/dev/null"
#endif

defaultConfig :: ConfigMonoid
defaultConfig = mempty

data DuplicateIdentifiers a = DuplicateIdentifiers [a]
instance (Show a, Typeable a) => Exception (DuplicateIdentifiers a)
instance Show a => Show (DuplicateIdentifiers a) where
  show (DuplicateIdentifiers ids) = "Duplicate identifiers: " <> show ids

doList :: [Benchmark] -> IO ()
doList bks =
    mapM_ Text.putStrLn $ bks^..folded.identifiers.to renderBenchmarkId

doRun :: SamplingStrategy -> [Benchmark] -> IO (HashMap BenchmarkId Sample)
doRun strategy bks = do
    let ids = bks^..folded.identifiers
    -- Better asymptotics than nub.
    unless (length (group (sort ids)) == length ids) $
      throwIO $ DuplicateIdentifiers [ n | n:_:_ <- group (sort ids) ]
    foldMap (runBenchmark (uniform strategy)) bks

doAnalyze
  :: Config -- ^ Hyperion config.
  -> Text -- ^ Package name.
  -> [Benchmark] -- ^ Benchmarks to be run.
  -> IO ()
doAnalyze Config{..} packageName bks = do
    executableName <- getProgName -- Name of the executable that launched the benches.
    h <- case configOutputPath of
      Nothing -> return IO.stdout
      Just path -> do
        dirExists <- doesDirectoryExist path
        if dirExists ||
           hasTrailingPathSeparator path
        then do
          let filename = (unpack packageName) <.> executableName <.> "json"
          createDirectoryIfMissing True path -- Creates the directory if needed.
          IO.openFile (path </> filename) IO.WriteMode
        else
          IO.openFile path IO.WriteMode
    results <- doRun configSamplingStrategy bks
    let strip
          | configRaw = id
          | otherwise = reportMeasurements .~ Nothing
        report = results & imapped %@~ analyze & mapped %~ strip
    now <- getCurrentTime
    BS.hPutStrLn h $ JSON.encode $
      json now Nothing report configExtraMetadata
    when configPretty (printReports report)
    maybe (return ()) (\_ -> IO.hClose h) configOutputPath

defaultMainWith
  :: ConfigMonoid -- ^ Preset Hyperion config.
  -> String -- ^ Package name, user provided.
  -> [Benchmark] -- ^ Benchmarks to be run.
  -> IO ()
defaultMainWith presetConfig packageName bks = do
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
        Run -> do _ <- doRun configSamplingStrategy bks; return ()
        Analyze -> doAnalyze config (pack packageName) bks

defaultMain
  :: String -- ^ Package name, user provided.
  -> [Benchmark] -- ^ Benchmarks to be run.
  -> IO ()
defaultMain = defaultMainWith defaultConfig
