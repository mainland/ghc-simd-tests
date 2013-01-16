{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Rbf.Double.VectorAlt (
    rbf
  ) where

import Data.Primitive.Multi
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

rbf :: Double -> U.Vector Double -> U.Vector Double -> Double
rbf nu x y =
    exp (-nu * msum (mmap square z))
  where
    z = mzipWith (-) x y
    square x = x * x

msum  ::  (G.PackedVector U.Vector a, U.Unbox a, Num a, Num (Multi a))
      =>  U.Vector a
      ->  a
{-# INLINE msum #-}
msum u = mfold' (+) 0 u

mmap  ::  (G.PackedVector U.Vector a, U.Unbox a, Num a, Num (Multi a))
      =>  (forall a . Num a => a -> a)
      ->  U.Vector a -> U.Vector a
{-# INLINE mmap #-}
mmap f u = U.mmap f f u

mfold'  ::  (G.PackedVector U.Vector a, U.Unbox a, Num a, Num (Multi a))
        =>  (forall a . Num a => a -> a -> a)
        ->  a
        ->  U.Vector a
        ->  a
{-# INLINE mfold' #-}
mfold' f z u = U.mfold' f f z u

mzipWith  ::  (G.PackedVector U.Vector a, U.Unbox a, Num a, Num (Multi a))
          =>  (forall a . Num a => a -> a -> a)
          ->  U.Vector a -> U.Vector a -> U.Vector a
{-# INLINE mzipWith #-}
mzipWith f u v = U.mzipWith f f u v
