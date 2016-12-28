{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hyperion where

import Control.Applicative
import Control.Exception (evaluate)
import Control.Monad.State (State, modify')
import Data.Monoid ((<>))
import Data.Vector (Vector)
import Data.Int (Int64)
import Control.DeepSeq

newtype Benchmarkable a = Benchmarkable { unBenchmarkable :: State (Int64 -> IO ()) a }
  deriving (Functor, Applicative, Monad)

data Resource r = Resource r

use :: Resource r -> Benchmarkable r
use (Resource x) = return x

data Benchmark where
  Bench :: String -> Benchmarkable () -> Benchmark
  Group :: String -> [Benchmark] -> Benchmark
  Shared :: NFData r => IO r -> (Resource r -> Benchmark) -> Benchmark
  Index :: Vector a -> Benchmark -> Benchmark

bench :: String -> Benchmarkable () -> Benchmark
bench = Bench

bgroup :: String -> [Benchmark] -> Benchmark
bgroup = Group

env :: NFData r => IO r -> (Resource r -> Benchmark) -> Benchmark
env = Shared

-- | Apply an argument to a function, and evaluate the result to weak
-- head normal form (WHNF).
whnf :: (a -> b) -> a -> Benchmarkable ()
whnf f x = Benchmarkable $ modify' (<> pureFunc id f x)
{-# INLINE whnf #-}

-- | Apply an argument to a function, and evaluate the result to head
-- normal form (NF).
nf :: NFData b => (a -> b) -> a -> Benchmarkable ()
nf f x = Benchmarkable $ modify' (<> pureFunc rnf f x)
{-# INLINE nf #-}

pureFunc :: (b -> c) -> (a -> b) -> a -> Int64 -> IO ()
pureFunc reduce f0 x0 = go f0 x0
  where go f x n
          | n <= 0    = return ()
          | otherwise = evaluate (reduce (f x)) >> go f x (n-1)
{-# INLINE pureFunc #-}

-- | Perform an action, then evaluate its result to head normal form.
-- This is particularly useful for forcing a lazy 'IO' action to be
-- completely performed.
nfIO :: NFData a => IO a -> Benchmarkable ()
nfIO m = Benchmarkable $ modify' (<> impure rnf m)
{-# INLINE nfIO #-}

-- | Perform an action, then evaluate its result to weak head normal
-- form (WHNF).  This is useful for forcing an 'IO' action whose result
-- is an expression to be evaluated down to a more useful value.
whnfIO :: IO a -> Benchmarkable ()
whnfIO m = Benchmarkable $ modify' (<> impure id m)
{-# INLINE whnfIO #-}

impure :: (a -> b) -> IO a -> Int64 -> IO ()
impure strategy a = go
  where go n
          | n <= 0    = return ()
          | otherwise = a >>= (evaluate . strategy) >> go (n-1)
{-# INLINE impure #-}
