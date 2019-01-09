{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Hyperion.Analysis
  ( Parameter(..)
  , identifiers
  , analyze
  ) where

import Control.Lens
  ( Contravariant(..)
  , Fold
  , ala
  , foldMapOf
  , folded
  , to
  , toListOf
  )

import Control.Lens.Each
import Data.Monoid
import Data.Traversable (for)
import Hyperion.Benchmark
import Hyperion.Internal
import Hyperion.Measurement
import Hyperion.Report
import Statistics.Regression
import qualified Data.Vector.Unboxed as UV

identifiers :: Fold Benchmark BenchmarkId
identifiers = go []
  where
    go :: [Component] -> Fold Benchmark BenchmarkId
    go comps f (Bench name _) = coerce $ f $ BenchmarkId $ comps <> [BenchC name]
    go comps f (Group name bks) = coerce $ (folded.go (comps <> [GroupC name])) f bks
    go comps f (Bracket _ _ g) = go comps f (g Empty)
    go comps f (Series xs g) =
      coerce $ for xs $ \x ->
        go (comps <> [SeriesC (Parameter x)]) f (g x)

    coerce :: (Contravariant f, Applicative f) => f a -> f b
    coerce = contramap (const ()) . fmap (const ())

analyze
  :: BenchmarkId -- ^ Benchmark identifier.
  -> Sample -- ^ Measurements.
  -> Report
analyze ident samp = Report
    { _reportBenchName = renderBenchmarkId ident
    , _reportBenchParams =
        map (\(Parameter x) -> fromEnum x) $ benchmarkParameters ident
    , _reportTimeInNanos =
        totalDuration / trueNumIterations 
    , _linearRegressionTime = slope
    , _linearRegressionConstant = yIntercept
    , _linearRegressionConfidence = determinationCoefficient
    , _reportCycles = Nothing
    , _reportAlloc = Nothing
    , _reportGarbageCollections = Nothing
    , _reportMeasurements = Just samp
    }
  where
    totalDuration =
      ala
        Sum
        (foldMapOf (measurements.each.duration.to realToFrac))
        samp
    trueNumIterations =
      ala
        Sum
        (foldMapOf (measurements.each.batchSize.to realToFrac))
        samp
    ([slope,yIntercept],determinationCoefficient) = (\(v,r) -> (UV.toList v,r)) $ olsRegress [batchSizesVect] durationsVect 
      where
        batchSizesVect = UV.fromList $ fromIntegral <$> toListOf (measurements.each.batchSize) samp
        durationsVect = UV.fromList $ fromIntegral <$> toListOf (measurements.each.duration) samp
