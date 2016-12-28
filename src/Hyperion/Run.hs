-- | Run a hierarchical benchmark suite, collecting results.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Hyperion.Run where

import Control.Monad (forM_)
import Control.Monad.Catch (bracket)
import Control.Monad.Trans (liftIO)
import Control.Monad.State.Strict (StateT, execStateT, modify')
import Data.Int
import Data.List (mapAccumR)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Vector.Unboxed as Unboxed
import Hyperion.Benchmark
import Hyperion.Internal
import Hyperion.Measurement
import qualified System.Clock as Clock
import System.Random (RandomGen(..))
import qualified System.Random.Shuffle as SRS

newtype Sample = Sample (Unboxed.Vector Measurement)

-- | Time an action.
chrono :: IO () -> IO Int64
chrono act = do
  start <- Clock.getTime Clock.Monotonic
  act
  end <- Clock.getTime Clock.Monotonic
  return $ fromIntegral $ Clock.toNanoSecs $ Clock.diffTimeSpec start end

fixed :: Int64 -> Batch () -> IO Sample
fixed batchSize batch = do
    duration <- chrono $ runBatch batch batchSize
    return $ Sample $ Unboxed.singleton Measurement{..}

runBenchmark :: (Batch () -> IO Sample) -> Benchmark -> IO (HashMap Text Sample)
runBenchmark sample bk0 = execStateT (go [] [] bk0) HashMap.empty
  where
    go :: [Text] -> [Text] -> Benchmark -> StateT (HashMap Text Sample) IO ()
    go pref suff (Bench name batch) = do
      results <- liftIO $ sample batch
      let key = Text.intercalate "/" pref <> "/" <> name <> ":" <> Text.intercalate ":" suff
      write key results
    go pref suff (Group name bks) = do
      forM_ bks $ \bk -> go (pref <> [name]) suff bk
    go pref suff (Bracket ini fini f) =
      bracket (liftIO ini) (liftIO . fini) (\x -> go pref suff (f (Resource x)))
    go pref suff (Series xs f) = do
      forM_ xs $ \x -> go pref ([Text.pack (show x)] <> suff) (f (Resource x))

    write k v = modify' (HashMap.insert k v)

-- | Convenience wrapper around 'SRS.shuffle'.
shuffle :: RandomGen g => g -> [a] -> [a]
shuffle gen xs = SRS.shuffle' xs (length xs) gen

splitn :: RandomGen g => Int -> g -> [g]
splitn n gen = snd $ mapAccumR (flip (const split)) gen [1..n]

reorder :: RandomGen g => g -> (g -> [Benchmark] -> [Benchmark]) -> Benchmark -> Benchmark
reorder gen0 shuf = go gen0
  where
    go _ bk@(Bench _ _) = bk
    go gen (Group name bks) = Group name (shuf gen (zipWith go (splitn (length bks) gen) bks))
    go gen (Bracket ini fini f) = Bracket ini fini (\x -> go gen (f x))
    go gen (Series xs f) = Series xs (\x -> go gen (f x))
