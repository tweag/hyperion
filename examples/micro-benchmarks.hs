#!/usr/bin/env stack
-- stack --no-nix-pure runghc --package hyperion

{-# LANGUAGE OverloadedLists #-}

module Main where

import Hyperion.Benchmark
import Hyperion.Main

fact, fib :: Int -> Int
fact n = if n == 0 then 1 else n * fact (n - 1)
fib n = case n of 1 -> 1; 2 -> 1; _ -> fib (n - 1) + fib (n - 2)

-- | Binomial coefficient
choose :: Int -> Int -> Int
choose n k = fact n `div` (fact k * fact (n - k))

benchmarks :: [Benchmark]
benchmarks =
    [ bench "id" (nf id ())
    , series [10..10] $ \n ->
        bgroup "pure-functions"
          [ bench "fact" (nf fact n)
          , bench "fib" (nf fib n)
          ]
    , series [1..4] $ \n ->
        series [1..n] $ \k ->
          bench "n choose k" $ nf (uncurry choose) (n, k)
    ]

main :: IO ()
main = defaultMain "hyperion-example-micro-benchmarks" benchmarks
