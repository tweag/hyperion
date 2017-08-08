{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Hyperion.Analysis
  ( namesOf
  , metadataOf
  , analyze
  ) where

import Control.Lens
  ( Contravariant(..)
  , Fold
  , ala
  , foldMapOf
  , folded
  , to
  )
import Control.Lens.Each
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Monoid
import Data.Traversable (for)
import Hyperion.Benchmark
import Hyperion.Internal
import Hyperion.Measurement
import Hyperion.Report

data Component = BenchC Metadata | GroupC Metadata | SeriesC Metadata

qualMeta :: [Component] -> Metadata
qualMeta = go ""
  where
    go index [BenchC txt] = txt <> index
    go index (GroupC txt : comps) = txt <> index <> "/" <> go "" comps
    go index (SeriesC txt : comps) = go (index <> ":" <> txt) comps
    go _ _ = error "qualMeta: Impossible"

namesOf :: Fold Benchmark Text
namesOf = metadataOf.benchmarkName

metadataOf :: Fold Benchmark Metadata
metadataOf = go []
  where
    go :: [Component] -> Fold Benchmark Metadata
    go comps f (Bench show' toJs _ _ x) =
      let md = Metadata (toJs x) (show' x)
      in coerce $ f (qualMeta (comps <> [BenchC md]))
    go comps f (Group name bks) =
      let md = mempty { _benchmarkName = name }
      in coerce $ (folded.go (comps <> [GroupC md])) f bks
    go comps f (Bracket _ _ g) = go comps f (g Empty)
    go comps f (Series xs g) =
      coerce $ for xs $ \x ->
        go (comps <> [SeriesC $ mempty {_benchmarkName = Text.pack (show x) }]) f (g Empty)
    go comps f (WithSampling _ bk) = go comps f bk

    coerce :: (Contravariant f, Applicative f) => f a -> f b
    coerce = contramap (const ()) . fmap (const ())

analyze
  :: Metadata -- ^ Benchmark metadata.
  -> Sample -- ^ Measurements.
  -> Report
analyze md samp = Report
    { _reportBenchName = _benchmarkName md
    , _reportTimeInNanos =
        totalDuration / trueNumIterations
    , _reportCycles = Nothing
    , _reportAlloc = Nothing
    , _reportGarbageCollections = Nothing
    , _reportMeasurements = Just samp
    , _reportUserData = _userData md
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
