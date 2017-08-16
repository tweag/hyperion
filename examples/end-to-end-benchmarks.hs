#!/usr/bin/env stack
-- stack --no-nix-pure runghc --package hyperion

{-# LANGUAGE OverloadedLists #-}

module Main where

import Hyperion.Benchmark
import Hyperion.Run
import Hyperion.Main
import System.Process (system)

benchmarks :: [Benchmark]
benchmarks =
    [ bgroup "roundrip"
        [ bench "ping" (nfIO (system "ping -c1 8.8.8.8 > /dev/null")) ]
    ]
  where

main :: IO ()
main = defaultMainWith config "hyperion-example-end-to-end" benchmarks
  where
    config = defaultConfig
      { configMonoidSamplingStrategy =
          return $ SamplingStrategy $ timebounded (repeat 10) fiveSecs
      }
    fiveSecs = 5 * 1000 * 1000 * 1000 -- in nanoseconds
