#!/usr/bin/env stack
-- stack --no-nix-pure runghc --package hyperion

{-# LANGUAGE OverloadedLists #-}

module Main where

import Hyperion.Benchmark
import Hyperion.Run
import Hyperion.Main
import System.Process (system)
import qualified System.Clock as Clock

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "roundtrip"
        [ bench "ping" (nfIO (system "ping -c1 8.8.8.8 > /dev/null")) ]
    ]

main :: IO ()
main = defaultMainWith config "hyperion-example-end-to-end" benchmarks
  where
    config = defaultConfig
      { configMonoidSamplingStrategy =
          pure $ timeBound (fromSeconds 5) (repeat 10)
      }
    fromSeconds :: Integer -> Clock.TimeSpec
    fromSeconds s = Clock.fromNanoSecs (s * (10^(9::Int)))
