{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Hyperion.Measurement
  ( Measurement(..)
  , batchSize
  , duration
  , Sample(..)
  , measurements
  ) where

import Data.Int
import Control.Lens.TH (makeLenses)
import Control.Monad (liftM)
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GMV
import qualified Data.Vector.Unboxed as UV

data Measurement = Measurement
  { _batchSize :: Int64
  , _duration :: Int64
  }
  deriving (Eq, Ord, Show)
makeLenses ''Measurement

instance UV.Unbox Measurement

newtype Sample = Sample { _measurements :: UV.Vector Measurement }
  deriving (Eq, Monoid, Ord, Show)
makeLenses ''Sample

newtype instance UV.MVector s Measurement = MV_Measurement (UV.MVector s (Int64, Int64))
newtype instance UV.Vector Measurement = V_Measurement  (UV.Vector (Int64, Int64))

instance GMV.MVector UV.MVector Measurement where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicInitialize #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}
  basicLength (MV_Measurement v) = GMV.basicLength v
  basicUnsafeSlice i n (MV_Measurement v) = MV_Measurement $ GMV.basicUnsafeSlice i n v
  basicOverlaps (MV_Measurement v1) (MV_Measurement v2) = GMV.basicOverlaps v1 v2
  basicUnsafeNew n = MV_Measurement `liftM` GMV.basicUnsafeNew n
  basicInitialize (MV_Measurement v) = GMV.basicInitialize v
  basicUnsafeReplicate n (Measurement x y) = MV_Measurement `liftM` GMV.basicUnsafeReplicate n (x,y)
  basicUnsafeRead (MV_Measurement v) i = uncurry Measurement `liftM` GMV.basicUnsafeRead v i
  basicUnsafeWrite (MV_Measurement v) i (Measurement x y) = GMV.basicUnsafeWrite v i (x,y)
  basicClear (MV_Measurement v) = GMV.basicClear v
  basicSet (MV_Measurement v) (Measurement x y) = GMV.basicSet v (x,y)
  basicUnsafeCopy (MV_Measurement v1) (MV_Measurement v2) = GMV.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_Measurement v1) (MV_Measurement v2) = GMV.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_Measurement v) n = MV_Measurement `liftM` GMV.basicUnsafeGrow v n

instance GV.Vector UV.Vector Measurement where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_Measurement v) = V_Measurement `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw (V_Measurement v) = MV_Measurement `liftM` GV.basicUnsafeThaw v
  basicLength (V_Measurement v) = GV.basicLength v
  basicUnsafeSlice i n (V_Measurement v) = V_Measurement $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_Measurement v) i = uncurry Measurement `liftM` GV.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_Measurement mv) (V_Measurement v) = GV.basicUnsafeCopy mv v
  elemseq _ (Measurement x y) z =
    GV.elemseq (undefined :: UV.Vector a) x $ GV.elemseq (undefined :: UV.Vector a) y z
