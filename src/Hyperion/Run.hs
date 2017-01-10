-- | Run a hierarchical benchmark suite, collecting results.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}


module Hyperion.Run
  ( -- * Run benchmarks
    runBenchmarkVaryingSizes
    -- * Benchmark transformations
  , shuffle
  , reorder
    -- * strategies
  , fixedBatchSizes
  , samples
  ) where

import Control.Lens (foldMapOf)
import Control.Monad (replicateM)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, bracket)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..))
import Data.Int
import Data.List (foldl', mapAccumR)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (ViewL((:<)), viewl)
import Data.Text (Text)
import qualified Data.Vector.Unboxed as Unboxed
import Hyperion.Analysis (namesOf)
import Hyperion.Benchmark
import Hyperion.Internal
import Hyperion.Measurement
import qualified System.Clock as Clock
import System.Random (RandomGen(..))
import qualified System.Random.Shuffle as SRS

-- | Time an action.
chrono :: IO () -> IO Int64
chrono act = do
  start <- Clock.getTime Clock.Monotonic
  act
  end <- Clock.getTime Clock.Monotonic
  return $ fromIntegral $ Clock.toNanoSecs $ Clock.diffTimeSpec start end

-- | Sample a batch once on each provided fixed size.
fixedBatchSizes :: [Int64] -> [Batch () -> IO Sample]
fixedBatchSizes = map fixed

-- | Sample once a batch of fixed size.
fixed :: Int64 -> Batch () -> IO Sample
fixed _batchSize batch = do
    _duration <- chrono $ runBatch batch _batchSize
    return $ Sample $ Unboxed.singleton Measurement{..}

-- | Run a samplin strategy @n@ times on multiple batch sizes.
samples :: Int64 -> [(Batch () -> IO Sample)] -> [Batch () -> IO Sample]
samples n f = map (\f' batch -> sample n f' batch) f

-- | Run a sampling strategy @n@ times.
sample :: Int64 -> (Batch () -> IO Sample) -> Batch () -> IO Sample
sample n f batch = mconcat <$> replicateM (fromIntegral n) (f batch)

-- | Local private copy of 'StateT' to hang our otherwise orphan 'Monoid'
-- instance to. This instance is missing from transformers.
newtype StateT' s m a = StateT' { unStateT' :: StateT s m a }
  deriving (Functor, Applicative, Monad, MonadCatch, MonadMask, MonadThrow, MonadState s, MonadTrans)

instance (Monad m, Monoid a) => Monoid (StateT' s m a) where
  mempty = lift (return mempty)
  mappend m1 m2 = mappend <$> m1 <*> m2

runBenchmark :: (Batch () -> IO Sample) -> Benchmark -> IO (HashMap Text Sample)
runBenchmark samplef bk0 =
  -- Ignore the names we find. Use fully qualified names accumulated from the
  -- lens defined above. The order is DFS in both cases.
  evalStateT (unStateT' (go bk0)) (foldMapOf namesOf return bk0)
  where
    go (Bench _ batch) = HashMap.singleton <$> pop <*> lift (samplef batch)
    go (Group _ bks) = foldMap go bks
    go (Bracket ini fini g) =
      bracket (lift ini) (lift . fini) (go . g . Resource)
    go (Series xs g) = foldMap (go . g . Resource) xs

    pop = do
      x :< xs <- viewl <$> get
      put xs
      return x

-- | Run a benchmark on varying sample sizes
runBenchmarkVaryingSizes :: [(Batch () -> IO Sample)] -> Benchmark -> IO (HashMap Text [Sample])
runBenchmarkVaryingSizes samplef bk0 = formatSamples $ sequence (map (\samplef' -> runBenchmark samplef' bk0) samplef)

-- | Set of functions to massage the data into the appropriate type
formatSamples :: IO([HashMap Text Sample]) -> IO (HashMap Text [Sample])
formatSamples samples = concatenateSamples $ expandSamples samples

expandSamples :: IO ([HashMap Text Sample]) -> IO ([HashMap Text [Sample]])
expandSamples samples = fmap (map (fmap (\x -> [x]))) samples

concatenateSamples :: IO ([HashMap Text [Sample]]) -> IO (HashMap Text [Sample])
concatenateSamples samples = fmap (foldl' (HashMap.unionWith(++)) HashMap.empty) samples

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
