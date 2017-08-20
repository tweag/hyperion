{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Hyperion.Internal where

import Control.Monad.State.Strict (State, execState)
import Data.Function (on)
import Data.Hashable (Hashable(..))
import Data.Int
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as Text

newtype Batch a = Batch { unBatch :: State (Int64 -> IO ()) a }
  deriving (Functor, Applicative, Monad)

-- | Run a batch of the given size.
runBatch :: Batch () -> Int64 -> IO ()
{-# INLINE runBatch #-}
runBatch bk = execState (unBatch bk) mempty

data Env r = Empty | Resource r

use :: Env r -> Batch r
use Empty = error "use called on empty environment."
use (Resource x) = return x

data Parameter = forall a. (Show a, Enum a) => Parameter a

instance Eq Parameter where
  (==) = (==) `on` \(Parameter x) -> fromEnum x

instance Ord Parameter where
  compare = compare `on` \(Parameter x) -> fromEnum x

data Component
  = BenchC Text
  | GroupC Text
  | SeriesC Parameter
  deriving (Eq, Ord)

newtype BenchmarkId = BenchmarkId [Component]
  deriving (Eq, Ord)


instance Hashable BenchmarkId where
  hashWithSalt s = hashWithSalt s . renderBenchmarkId

instance Show BenchmarkId where
  show = Text.unpack . renderBenchmarkId

renderBenchmarkId :: BenchmarkId -> Text
renderBenchmarkId (BenchmarkId comps0) = go "" comps0
  where
    go index [BenchC txt] = txt <> index
    go index (GroupC txt : comps) = txt <> index <> "/" <> go "" comps
    go index (SeriesC (Parameter x) : comps) =
        go (index <> ":" <> Text.pack (show x)) comps
    go _ _ = error "renderBenchmarkId: Impossible"

benchmarkParameters :: BenchmarkId -> [Parameter]
benchmarkParameters (BenchmarkId comps) =
    concatMap (\case SeriesC x -> [x]; _ -> []) comps
