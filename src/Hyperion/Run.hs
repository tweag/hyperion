-- | Run a hierarchical benchmark suite, collecting results.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}


module Hyperion.Run
  ( -- * Run benchmarks
    runBenchmark
  , runBenchmarkWithConfig
    -- * Benchmark transformations
  , shuffle
  , reorder
    -- * Sampling strategies
  , SamplingStrategy(..)
  , fixed
  , sample
  , geometric
  , timebounded
    -- * Strategy helpers
  , geometricSeries
  ) where

import Control.DeepSeq
import Control.Exception (evaluate)
import Control.Lens (foldMapOf)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, bracket)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..))
import Data.Int
import Data.List (mapAccumR)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (ViewL((:<)), viewl)
import qualified Data.Vector.Unboxed as Unboxed
import Hyperion.Analysis (identifiers)
import Hyperion.Benchmark
import Hyperion.Internal
import Hyperion.Measurement
import qualified System.Clock as Clock
import System.Random (RandomGen(..))
import qualified System.Random.Shuffle as SRS

-- | Local private copy of 'StateT' to hang our otherwise orphan 'Monoid'
-- instance to. This instance is missing from transformers.
newtype StateT' s m a = StateT' { unStateT' :: StateT s m a }
  deriving (Functor, Applicative, Monad, MonadCatch, MonadMask, MonadThrow, MonadState s, MonadTrans)

instance (Monad m, Monoid a) => Monoid (StateT' s m a) where
  mempty = lift (return mempty)
  mappend m1 m2 = mappend <$> m1 <*> m2

-- | Default way of running benchmarks.
-- Default is 100 samples, for each batch size from 1 to 20 with a geometric
-- progression of 1.2.
runBenchmark :: Benchmark -> IO (HashMap BenchmarkId Sample)
runBenchmark = runBenchmarkWithConfig (geometricBatches 100 20 1.2)

newtype SamplingStrategy = SamplingStrategy (Batch () -> IO Sample)
  deriving (Monoid)

-- | Runs the benchmarks with the provided config.
-- Only to be used if the default running configuration does not suit you.
runBenchmark
  :: SamplingStrategy
  -- ^ Name indexed batch sampling strategy.
  -> Benchmark
  -- ^ Benchmark to be run.
  -> IO (HashMap BenchmarkId Sample)
runBenchmarkWithConfig samplingConf bk0 =
  -- Ignore the identifiers we find. Use fully qualified identifiers
  -- accumulated from the lens defined above. The order is DFS in both cases.
  evalStateT (unStateT' (go bk0)) (foldMapOf identifiers return bk0)
  where
    go (Bench _ batch) = HashMap.singleton <$> pop <*> lift (cfg batch)
    go (Group _ bks) = foldMap go bks
    go (Bracket ini fini g) =
      bracket (lift (ini >>= evaluate . force)) (lift . fini) (go . g . Resource)
    go (Series xs g) = foldMap (go . g) xs

    pop = do
      x :< xs <- viewl <$> get
      put xs
      return x

-- | Time an action.
chrono :: IO () -> IO Int64
chrono act = do
    start <- Clock.getTime Clock.Monotonic
    act
    end <- Clock.getTime Clock.Monotonic
    return $ fromIntegral $ Clock.toNanoSecs $ Clock.diffTimeSpec start end

-- | Sample once a batch of fixed size.
fixed :: Int64 -> SamplingStrategy
fixed _batchSize = SamplingStrategy $ \batch -> do
    _duration <- chrono $ runBatch batch _batchSize
    return $ Sample $ Unboxed.singleton Measurement{..}

-- | Run a sampling strategy @n@ times.
sample :: Int64 -> SamplingStrategy -> SamplingStrategy
sample n strategy = mconcat $ replicate (fromIntegral n) strategy

-- | Sampling strategy that creates samples of the specified sizes with a time
-- bound. Sampling stops when either a sample has been sampled for each size or
-- when the total benchmark time is greater than the specified time bound.
--
-- The actual amount of time spent may be longer since hyperion will always
-- wait for a 'Sample' of a given size to complete.
timebounded
  :: [Int64] -- ^ Sample sizes; may be infinite
  -> Clock.TimeSpec -- ^ Time bound
  -> Batch () -> IO Sample
timebounded batchSizes maxTime batch = do
    start <- Clock.getTime Clock.Monotonic
    go start batchSizes mempty
  where
    go start (_batchSize:bss) smpl = do
      _duration <- chrono $ runBatch batch _batchSize
      let smpl' = smpl `mappend` (Sample $ Unboxed.singleton Measurement{..})
      now <- Clock.getTime Clock.Monotonic
      if Clock.diffTimeSpec start now > maxTime
        then return smpl'
        else go start bss smpl'
    go _ _ s = return s

-- | Batching strategy, following a geometric progression from 1
-- to the provided limit, with the given ratio.
geometric
  :: Int64 -- ^ Sample size.
  -> Int64 -- ^ Max batch size.
  -> Double -- ^ Ratio of geometric progression.
  -> SamplingStrategy
geometric nSamples limit ratio =
    foldMap (\size -> sample nSamples (fixed size)) (geometricSeries ratio limit)

geometricSeries
  :: Double -- ^ Geometric progress.
  -> Int64 -- ^ End of the series.
  -> [Int64]
geometricSeries ratio limit =
    if ratio > 1
    then
      takeWhile (<= limit) $
      squish $
      map truncate $
      map (ratio^) ([1..] :: [Int])
    else
      error "Geometric ratio must be bigger than 1"

-- | Our series starts its growth very slowly when we begin at 1, so we
-- eliminate repeated values.
-- NOTE: taken from Criterion.
squish :: (Eq a) => [a] -> [a]
squish ys = foldr go [] ys
  where go x xs = x : dropWhile (==x) xs

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
