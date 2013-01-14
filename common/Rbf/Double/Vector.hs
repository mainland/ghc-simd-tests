{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Rbf.Double.Vector (
    rbf
  ) where

import Data.Primitive.Multi
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

rbf :: Double -> U.Vector Double -> U.Vector Double -> Double
rbf nu v w =
    exp (-nu * msum (mzipWith norm v w))
  where
    square x = x * x
    norm x y = square (x-y)

msum  ::  (G.PackedVector U.Vector a, U.Unbox a, Num a, Num (Multi a))
      =>  U.Vector a
      ->  a
{-# INLINE msum #-}
msum u = mfold' (+) 0 u

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
