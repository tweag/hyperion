#!/usr/bin/env stack
-- stack --no-nix-pure runghc

{-# LANGUAGE OverloadedLists #-}

module Main where

import Hyperion.Benchmark
import Hyperion.Run
import Hyperion.Main
import System.Process (system)

fact, fib :: Int -> Int
fact n = if n == 0 then 1 else n * fact (n - 1)
fib n = case n of 1 -> 1; 2 -> 1; _ -> fib (n - 1) + fib (n - 2)

choose :: Int -> Int -> Int
choose n k = fact n `div` (fact k * fact (n - k))

benchmarks :: [Benchmark]
benchmarks =
    [ bench "id" (nf id ())
    , series [10..10] $ \n ->
        bgroup "pure-functions"
          [ bench "fact" (use n >>= nf fact)
          , bench "fib" (use n >>= nf fib)
          ]
    , withSampling (timebounded (repeat 10) fiveSecs) $ bgroup "roundrip"
        [ bench "ping" (nfIO (system "ping -c1 8.8.8.8 > /dev/null")) ]
    , benchMany
        "n choose k"
        [(x,y) | x <- [1..4], y <- [1..4], x >= y]
        (nf $ uncurry choose)
    ]
  where
    fiveSecs = 5 * 1000 * 1000 * 1000 -- in nanoseconds

main :: IO ()
main = defaultMain "hyperion-example-package" benchmarks
