{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Hyperion.Analysis
  ( names
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

data Component = BenchC Text | GroupC Text | SeriesC Text

qualName :: [Component] -> Text
qualName = go ""
  where
    go index [BenchC txt] = txt <> index
    go index (GroupC txt : comps) = txt <> index <> "/" <> go "" comps
    go index (SeriesC txt : comps) = go (index <> ":" <> txt) comps
    go _ _ = error "qualName: Impossible"

names :: Fold Benchmark Text
names = go []
  where
    go :: [Component] -> Fold Benchmark Text
    go comps f (Bench name _) = coerce $ f (qualName (comps <> [BenchC name]))
    go comps f (Group name bks) = coerce $ (folded.go (comps <> [GroupC name])) f bks
    go comps f (Bracket _ _ g) = go comps f (g Empty)
    go comps f (Series xs g) =
      coerce $ for xs $ \x ->
        go (comps <> [SeriesC (Text.pack (show x))]) f (g x)
    go comps f (WithSampling _ bk) = go comps f bk

    coerce :: (Contravariant f, Applicative f) => f a -> f b
    coerce = contramap (const ()) . fmap (const ())

analyze
  :: Text -- ^ Benchmark name.
  -> Sample -- ^ Measurements.
  -> Report
analyze name samp = Report
    { _reportBenchName = name
    , _reportTimeInNanos =
        totalDuration / trueNumIterations
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
