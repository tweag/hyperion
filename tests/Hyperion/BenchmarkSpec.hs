{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Hyperion.BenchmarkSpec where

import Control.Lens ((^..), foldOf, to)
import Data.List (nub)
import Hyperion.Analysis
import Hyperion.Benchmark
import Hyperion.Run
import Test.Hspec
import Test.QuickCheck
  ( Arbitrary(..)
  , (==>)
  , elements
  , oneof
  , property
  , scale
  )
import Test.QuickCheck.Monadic (assert, monadicIO, run)

benchmarkNames, groupNames :: [String]
benchmarkNames = ["foo", "bar", "baz"]
groupNames = ["one", "two"]

instance Arbitrary Benchmark where
  arbitrary = scale (min 4) $ oneof
    [ bench <$>
        elements benchmarkNames <*>
        pure (nf id ())
    , bgroup <$>
        elements groupNames <*>
        arbitrary
    , env <$>
        pure (return ()) <*>
        pure (\_ -> return ()) <*>
        const <$> scale (max 0 . pred) arbitrary
    , series <$>
        pure [1,2,3 :: Int] <*>
        const <$> scale (max 0 . pred) arbitrary
    ]

hasSeries :: Benchmark -> Bool
hasSeries b = any (==':') (foldOf (identifiers.to show) b)

spec :: Spec
spec = do
    describe "runBenchmark" $ do
      it "returns as many samples as there are benchmarks" $ property $ \b ->
        not (hasSeries b) ==>
        monadicIO $ do
          results <- run $ runBenchmarkWithConfig (fixed 1) b
          assert (length results == length (nub (b^..identifiers)))
