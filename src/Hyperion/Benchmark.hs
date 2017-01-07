{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hyperion.Benchmark
  ( -- * Benchmarks
    Benchmark(..)
  , bench
  , bgroup
  , env
  , series
  -- * Batches
  , Batch
  , runBatch
  , nf
  , nfIO
  , whnf
  , whnfIO
  -- * Environments
  , Env
  , use
  ) where

import Control.Exception (evaluate)
import Control.Monad.State.Strict (modify')
import Data.Monoid
import Data.Vector (Vector)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as Text
import Control.DeepSeq
import Hyperion.Internal

data Benchmark where
  Bench :: Text -> Batch () -> Benchmark
  Group :: Text -> [Benchmark] -> Benchmark
  Bracket :: NFData r => IO r -> (r -> IO ()) -> (Env r -> Benchmark) -> Benchmark
  Series :: Show a => Vector a -> (Env a -> Benchmark) -> Benchmark

sp :: ShowS
sp = showChar ' '

instance Show Benchmark where
  showsPrec _ (Bench name _) =
      showString "Bench" . sp . shows name . sp . showString "_"
  showsPrec x (Group name bks) =
      showString "Group" . sp . shows name . sp . showsPrec x bks
  showsPrec x (Bracket _ _ f) =
      showString "Bracket" . sp . showString "(\\_ -> " . showsPrec x (f Empty) . showString ")"
  showsPrec x (Series xs f) =
      showString "Series" . sp . shows xs . sp . showString "(\\_ -> " . showsPrec x (f Empty) . showString ")"

bench :: String -> Batch () -> Benchmark
bench name batch = Bench (Text.pack name) batch

bgroup :: String -> [Benchmark] -> Benchmark
bgroup name bks = Group (Text.pack name) bks

series :: Show a => Vector a -> (Env a -> Benchmark) -> Benchmark
series = Series

env
  :: NFData r
  => IO r -- ^ Acquire resource
  -> (r -> IO ()) -- ^ Finalize resource
  -> (Env r -> Benchmark)
  -> Benchmark
env = Bracket

-- | Apply an argument to a function, and evaluate the result to weak
-- head normal form (WHNF).
whnf :: (a -> b) -> a -> Batch ()
{-# INLINE whnf #-}
whnf f x = Batch $ modify' (<> pureFunc id f x)

-- | Apply an argument to a function, and evaluate the result to head
-- normal form (NF).
nf :: NFData b => (a -> b) -> a -> Batch ()
{-# INLINE nf #-}
nf f x = Batch $ modify' (<> pureFunc rnf f x)

pureFunc :: (b -> c) -> (a -> b) -> a -> Int64 -> IO ()
{-# INLINE pureFunc #-}
pureFunc reduce f0 x0 = go f0 x0
  where go f x n
          | n <= 0    = return ()
          | otherwise = evaluate (reduce (f x)) >> go f x (n-1)

-- | Perform an action, then evaluate its result to head normal form.
-- This is particularly useful for forcing a lazy 'IO' action to be
-- completely performed.
nfIO :: NFData a => IO a -> Batch ()
{-# INLINE nfIO #-}
nfIO m = Batch $ modify' (<> impure rnf m)

-- | Perform an action, then evaluate its result to weak head normal
-- form (WHNF).  This is useful for forcing an 'IO' action whose result
-- is an expression to be evaluated down to a more useful value.
whnfIO :: IO a -> Batch ()
{-# INLINE whnfIO #-}
whnfIO m = Batch $ modify' (<> impure id m)

impure :: (a -> b) -> IO a -> Int64 -> IO ()
{-# INLINE impure #-}
impure strategy a = go
  where go n
          | n <= 0    = return ()
          | otherwise = a >>= (evaluate . strategy) >> go (n-1)
