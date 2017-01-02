-- | Run a hierarchical benchmark suite, collecting results.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}


module Hyperion.Run
  ( -- * Run benchmarks
    runBenchmark
    -- * Benchmark transformations
  , shuffle
  , reorder
    -- * strategies
  , fixed
  , sample
  ) where

import Control.Lens (foldMapOf)
import Control.Monad (replicateM)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow, bracket)
import Control.Monad.State.Class (MonadState)
import Control.Monad.State.Strict (StateT, evalStateT, get, put)
import Control.Monad.Trans (MonadTrans(..))
import Data.Int
import Data.List (mapAccumR)
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

-- | Sample once a batch of fixed size.
fixed :: Int64 -> Batch () -> IO Sample
fixed _batchSize batch = do
    _duration <- chrono $ runBatch batch _batchSize
    return $ Sample $ Unboxed.singleton Measurement{..}

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
