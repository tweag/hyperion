{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
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
import Control.Lens ((&), (.~), (%~), (%@~), (^..), folded, imapped, mapped)
import Control.Monad (unless, when)
import Data.HashMap.Strict (HashMap)
import Data.List (group, sort)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Text (pack, Text, unpack)
import qualified Data.Text.IO as Text
import Data.Time (getCurrentTime)
import Data.Version (showVersion)
import Hyperion.Analysis
import Hyperion.Benchmark
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
  }

instance Monoid ConfigMonoid where
  mempty =
    ConfigMonoid
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

data Config = Config
  { configOutputPath :: Maybe FilePath
  , configMode :: Mode
  , configPretty :: Bool
  , configRaw :: Bool
  }

fromFirst :: a -> First a -> a
fromFirst x = fromMaybe x . getFirst

configFromMonoid :: ConfigMonoid -> Config
configFromMonoid ConfigMonoid{..} = Config
    { configOutputPath = getFirst configMonoidOutputPath
    , configMode = fromFirst Analyze configMonoidMode
    , configPretty = fromFirst False configMonoidPretty
    , configRaw = fromFirst False configMonoidRaw
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
     pure ConfigMonoid{..}

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

data DuplicateNames = DuplicateNames [Text]
instance Exception DuplicateNames
instance Show DuplicateNames where
  show (DuplicateNames nms) = "Duplicate names: " <> show nms

doList :: [Benchmark] -> IO ()
doList bks = mapM_ Text.putStrLn $ bks^..folded.names

doRun :: [Benchmark] -> IO (HashMap Text Sample)
doRun bks = do
    let nms = bks^..folded.names
    -- Better asymptotics than nub.
    unless (length (group (sort nms)) == length nms) $
      throwIO $ DuplicateNames [ n | n:_:_ <- group (sort nms) ]
    foldMap runBenchmark bks

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
    results <- doRun bks
    let strip
          | configRaw = id
          | otherwise = reportMeasurements .~ Nothing
        report = results & imapped %@~ analyze & mapped %~ strip
    now <- getCurrentTime
    BS.hPutStrLn h $ JSON.encode $ json now Nothing report
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
        Run -> do _ <- doRun bks; return ()
        Analyze -> doAnalyze config (pack packageName) bks

defaultMain
  :: String -- ^ Package name, user provided.
  -> [Benchmark] -- ^ Benchmarks to be run.
  -> IO ()
defaultMain = defaultMainWith defaultConfig
