{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hyperion.Internal where

import Control.Monad.State.Strict (State, execState)
import Data.Int

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
