{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
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
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (group, sort)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (pack, Text, unpack)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Time (getCurrentTime)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Generics.Deriving.Monoid (memptydefault, mappenddefault)
import Hyperion.Analysis
import Hyperion.Benchmark
import Hyperion.Internal
import Hyperion.Measurement
import Hyperion.PrintReport
import Hyperion.Report
import Hyperion.Run
import qualified Options.Applicative as Options
import Paths_hyperion (version)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist)
import System.Environment (getProgName)
import System.FilePath ((</>), (<.>))
import System.FilePath.Posix (hasTrailingPathSeparator)
import qualified System.IO as IO

data Mode = Version | List | Run | Analyze
  deriving (Eq, Ord, Show)

data ConfigMonoid = ConfigMonoid
  { configMonoidOutputPath :: First FilePath
  , configMonoidMode :: First Mode
  , configMonoidPretty :: First Bool
  , configMonoidRaw :: First Bool
  , configMonoidSamplingStrategy :: First SamplingStrategy
  , configMonoidUserMetadata :: JSON.Object
  , configMonoidPattern :: First Text
  } deriving (Generic, Show)

instance Monoid ConfigMonoid where
  mempty = memptydefault
  mappend = mappenddefault

data Config = Config
  { configOutputPath :: Maybe FilePath
  , configMode :: Mode
  , configPretty :: Bool
  , configRaw :: Bool
  , configSamplingStrategy :: SamplingStrategy
  , configUserMetadata :: JSON.Object
  , configPattern :: Maybe Text
  } deriving (Generic, Show)

fromFirst :: a -> First a -> a
fromFirst x = fromMaybe x . getFirst

configFromMonoid :: ConfigMonoid -> Config
configFromMonoid ConfigMonoid{..} = Config
    { configOutputPath = getFirst configMonoidOutputPath
    , configMode = fromFirst Analyze configMonoidMode
    , configPretty = fromFirst False configMonoidPretty
    , configRaw = fromFirst False configMonoidRaw
    , configSamplingStrategy = fromFirst defaultStrategy configMonoidSamplingStrategy
    , configUserMetadata = configMonoidUserMetadata
    , configPattern = getFirst configMonoidPattern
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
     configMonoidUserMetadata <-
       HashMap.fromList <$> many
         (Options.option
            parseKV
            (Options.long "arg" <>
             Options.metavar "KEY:VAL" <>
             Options.help "Extra metadata to include in the report, in the format key:value."))
     configMonoidPattern <-
       First <$> optional
         (pack <$> Options.strOption
            (Options.long "pattern" <>
             Options.short 'p' <>
             Options.help "Select only tests that match pattern (infix)"))
     pure ConfigMonoid{..}
  where
     -- TODO allow setting this from CLI.
    configMonoidSamplingStrategy = First Nothing
    parseKV = do
      txt <- Text.pack <$> Options.str
      case Text.splitOn ":" txt of
        [x,y] -> pure (x, JSON.String y)
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

-- | Create a 'BenchmarkPredicate' that selects only the benchmarks whose names
-- contain the given 'Text'.
mkTextPredicate :: Text.Text -> BenchmarkPredicate
mkTextPredicate txt = BenchmarkPredicate $ \bid ->
    Text.isInfixOf txt $ renderBenchmarkId bid

doRun
  :: SamplingStrategy
  -> Maybe BenchmarkPredicate
  -- ^ Predicate selecting which benchmarks to run. If 'Nothing', all
  -- benchmarks are run.
  -> [Benchmark]
  -> IO (HashMap BenchmarkId Sample)
doRun strategy prd bks = do
    let ids = bks^..folded.identifiers
    -- Better asymptotics than nub.
    unless (length (group (sort ids)) == length ids) $
      throwIO $ DuplicateIdentifiers [ n | n:_:_ <- group (sort ids) ]
    let strat = maybe uniform filtered prd
    foldMap (runBenchmark (strat strategy)) bks

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
    results <- doRun configSamplingStrategy
      (mkTextPredicate <$> configPattern) bks
    let strip
          | configRaw = id
          | otherwise = reportMeasurements .~ Nothing
        report = results & imapped %@~ analyze & mapped %~ strip
    now <- getCurrentTime
    let -- TODO Use output of hostname(1) as reasonable default.
        hostId = Nothing :: Maybe Text
        metadata =
          -- Append user metadata at the end so that the user can rewrite
          -- @timestamp@, for instance.
          HashMap.fromList [ "timestamp" JSON..= now, "location" JSON..= hostId ]
            <> configUserMetadata
    BS.hPutStrLn h $ JSON.encode $ json metadata report
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
        Run -> do
          _ <- doRun configSamplingStrategy (mkTextPredicate <$> configPattern) bks
          return ()
        Analyze -> doAnalyze config (pack packageName) bks

defaultMain
  :: String -- ^ Package name, user provided.
  -> [Benchmark] -- ^ Benchmarks to be run.
  -> IO ()
defaultMain = defaultMainWith defaultConfig
