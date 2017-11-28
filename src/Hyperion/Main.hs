{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hyperion.Main
  ( defaultMain
  , Mode(..)
  , ConfigMonoid(..)
  , ReportOutput(..)
  , nullOutputPath
  , defaultConfig
  , defaultMainWith
  ) where

import Control.Applicative
import Control.Exception (Exception, throwIO, bracket)
import Control.Lens ((&), (.~), (%~), (^..), folded, mapped, to)
import Control.Monad (unless, mzero, void)
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict as HashMap
import Data.List (group, sort)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set
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

-- | Specify a particular way of reporting the benchmark results.
data ReportOutput a = ReportPretty | ReportJson a | ReportJsonFlat a
  deriving (Functor, Eq, Ord, Show)

-- | Context information about the benchmark.
data ContextInfo = ContextInfo
  { contextPackageName :: Text
  , contextExecutableName :: Text
  }

data ConfigMonoid = ConfigMonoid
  { configMonoidReportOutputs :: [ReportOutput FilePath]
  , configMonoidMode :: First Mode
  , configMonoidRaw :: First Bool
  , configMonoidSamplingStrategy :: First SamplingStrategy
  , configMonoidUserMetadata :: JSON.Object
  , configMonoidSelectorPatterns :: [Text]
  } deriving (Generic, Show)

instance Monoid ConfigMonoid where
  mempty = memptydefault
  mappend = mappenddefault

data Config = Config
  { configReportOutputs :: Set (ReportOutput FilePath)
  , configMode :: Mode
  , configRaw :: Bool
  , configSamplingStrategy :: SamplingStrategy
  , configUserMetadata :: JSON.Object
  , configSelectorPatterns :: [Text]
  } deriving (Generic, Show)

fromFirst :: a -> First a -> a
fromFirst x = fromMaybe x . getFirst

configFromMonoid :: ConfigMonoid -> Config
configFromMonoid ConfigMonoid{..} = Config
    { configReportOutputs =
        if null configMonoidReportOutputs
        then Set.singleton ReportPretty
        else Set.fromList configMonoidReportOutputs
    , configMode = fromFirst Analyze configMonoidMode
    , configRaw = fromFirst False configMonoidRaw
    , configSamplingStrategy = fromFirst defaultStrategy configMonoidSamplingStrategy
    , configUserMetadata = configMonoidUserMetadata
    , configSelectorPatterns = configMonoidSelectorPatterns
    }

options :: Options.Parser ConfigMonoid
options = do
     configMonoidReportOutputs <- many reportOutputParse
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
     configMonoidUserMetadata <-
       HashMap.fromList <$> many
         (Options.option
            parseKV
            (Options.long "arg" <>
             Options.metavar "KEY:VAL" <>
             Options.help "Extra metadata to include in the report, in the format key:value."))
     configMonoidSelectorPatterns <-
       many
         (pack <$> Options.argument Options.str
            (Options.metavar "NAME..." ))
     pure ConfigMonoid{..}
  where
     -- TODO allow setting this from CLI.
    configMonoidSamplingStrategy = First Nothing
    parseKV = do
      txt <- Text.pack <$> Options.str
      case Text.splitOn ":" txt of
        [x,y] -> pure (x, JSON.String y)
        _ -> mzero

reportOutputParse :: Options.Parser (ReportOutput FilePath)
reportOutputParse =
    (ReportPretty <$ Options.flag' ()
       (Options.long "pretty" <>
        Options.help "Pretty prints the measurements on stdout.")) <|>
    (ReportJson <$> Options.strOption
      (Options.long "json" <>
       Options.short 'j' <>
       Options.help (unwords
          ["Where to write the json benchmarks output."
          ,"Can be a file name, a directory name or '-' for stdout."
          ]) <>
       Options.metavar "PATH")) <|>
    (ReportJsonFlat <$> Options.strOption
      (Options.long "flat" <>
       Options.short 'f' <>
       Options.help (unwords
          ["Where to write the json benchmarks output."
          ,"Can be a file name, a directory name or '-' for stdout."
          ]) <>
       Options.metavar "PATH"))

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

-- | Derive a 'SamplingStrategy' indexed by 'BenchmarkId' from the current
-- configuration.
indexedStrategy :: Config -> (BenchmarkId -> Maybe SamplingStrategy)
indexedStrategy Config{..} = case configSelectorPatterns of
    [] -> uniform configSamplingStrategy
    patts -> filtered f configSamplingStrategy
      where
        f bid = any (`Text.isPrefixOf` renderBenchmarkId bid) patts

doRun
  :: (BenchmarkId -> Maybe SamplingStrategy)
  -> [Benchmark]
  -> IO [(BenchmarkId, Sample)]
doRun strategy bks = do
    let ids = bks^..folded.identifiers
    -- Better asymptotics than nub.
    unless (length (group (sort ids)) == length ids) $
      throwIO $ DuplicateIdentifiers [ n | n:_:_ <- group (sort ids) ]
    foldMap (runBenchmark strategy) bks

-- | Print the report.
printReport
  :: ReportOutput IO.Handle
  -> JSON.Object -- ^ Metadata
  -> [Report]
  -> IO ()
-- XXX: should we print user metadata in pretty mode as well?
printReport ReportPretty _ report = printReports report
printReport (ReportJson h) metadata report =
    BS.hPutStrLn h $ JSON.encode $
      json metadata report
printReport (ReportJsonFlat h) metadata report =
    BS.hPutStrLn h $ JSON.encode $
      jsonFlat metadata report

-- | Open a 'Handle' for given report (if needed).
openReportHandle
  :: ContextInfo
  -> ReportOutput FilePath -> IO (ReportOutput IO.Handle)
openReportHandle _ ReportPretty = pure ReportPretty
openReportHandle cinfo (ReportJson path) =
    ReportJson <$> openReportFileHandle cinfo path
openReportHandle cinfo (ReportJsonFlat path) =
    ReportJsonFlat <$> openReportFileHandle cinfo path

openReportFileHandle :: ContextInfo -> FilePath -> IO IO.Handle
openReportFileHandle _ "-" = pure IO.stdout
openReportFileHandle cinfo path = do
    let packageName = unpack $ contextPackageName cinfo
        executableName = unpack $ contextExecutableName cinfo
    dirExists <- doesDirectoryExist path
    if dirExists ||
       hasTrailingPathSeparator path
    then do
      let filename = packageName <.> executableName <.> "json"
      createDirectoryIfMissing True path -- Creates the directory if needed.
      IO.openFile (path </> filename) IO.WriteMode
    else
      IO.openFile path IO.WriteMode

closeReportHandle :: ReportOutput IO.Handle -> IO ()
closeReportHandle ReportPretty = return ()
closeReportHandle (ReportJson h) = IO.hClose h
closeReportHandle (ReportJsonFlat h) = IO.hClose h

doAnalyze
  :: Config -- ^ Hyperion config.
  -> ContextInfo -- ^ Benchmark context information.
  -> [Benchmark] -- ^ Benchmarks to be run.
  -> IO ()
doAnalyze Config{..} cinfo bks = do
    results <- doRun (indexedStrategy Config{..}) bks
    let strip
          | configRaw = id
          | otherwise = reportMeasurements .~ Nothing
        report = results & mapped %~ uncurry analyze & mapped %~ strip
    now <- getCurrentTime
    let -- TODO Use output of hostname(1) as reasonable default.
        hostId = Nothing :: Maybe Text
        metadata =
          configUserMetadata
          -- Prepend user metadata so that the user can rewrite @timestamp@,
          -- for instance.
            <> HashMap.fromList [ "timestamp" JSON..= now, "location" JSON..= hostId ]
    void $ bracket
      (mapM (openReportHandle cinfo)
        $ Set.toList configReportOutputs)
      (mapM_ closeReportHandle)
      (mapM (\h -> printReport h metadata report))

defaultMainWith
  :: ConfigMonoid -- ^ Preset Hyperion config.
  -> String -- ^ Package name, user provided.
  -> [Benchmark] -- ^ Benchmarks to be run.
  -> IO ()
defaultMainWith presetConfig packageName bks = do
    executableName <- getProgName -- Name of the executable that launched the benches.
    cmdlineConfig <-
      Options.execParser
        (Options.info
          (Options.helper <*> options)
          Options.fullDesc)
    let config = configFromMonoid (cmdlineConfig <> presetConfig)
        cinfo = ContextInfo
          { contextPackageName = pack packageName
          , contextExecutableName = pack executableName
          }
    case config of
      Config{..} -> case configMode of
        Version -> putStrLn $ "Hyperion " <> showVersion version
        List -> doList bks
        Run -> do
          _ <- doRun (indexedStrategy config) bks
          return ()
        Analyze -> doAnalyze config cinfo bks

defaultMain
  :: String -- ^ Package name, user provided.
  -> [Benchmark] -- ^ Benchmarks to be run.
  -> IO ()
defaultMain = defaultMainWith defaultConfig
