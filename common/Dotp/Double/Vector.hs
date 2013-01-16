{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Dotp.Double.Vector (
    dotp
  ) where

import Data.Primitive.Multi
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

dotp :: U.Vector Double -> U.Vector Double -> Double
dotp v w = mfold' (+) 0 $ mzipWith (*) v w
--    U.mfold' (+) (+) 0 $ U.mzipWith (*) (*) v w

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
