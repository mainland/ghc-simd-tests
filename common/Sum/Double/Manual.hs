{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -W -Werror #-}

module Sum.Double.Manual (
    sum
  ) where

import Prelude hiding (sum)

import Data.Primitive.Multi
import qualified Data.Vector.Unboxed as U

sum :: U.Vector Double -> Double
sum u =
    n `seq` k `seq` m `seq` loop 0 0
  where
    m :: Int
    m = multiplicity (undefined :: Multi Double)

    n, k :: Int
    n = U.length u
    k = n - n `rem` m

    loop :: Multi Double -> Int -> Double
    {-# INLINE loop #-}
    loop !z i | i >= k =
        loop1 (reduce z) i

    loop !z i =
        loop (z + x) (i+m)
      where
        x :: Multi Double
        x = U.munsafeIndex u i

    loop1 :: Double -> Int -> Double
    {-# INLINE loop1 #-}
    loop1 !z i | i >= n =
        z

    loop1 !z i =
        loop1 (z + x) (i+1)
      where
        x :: Double
        x = U.unsafeIndex u i

    reduce :: Multi Double -> Double
    {-# INLINE reduce #-}
    reduce md =
        multifold (+) 0 md
{-
    reduce (MultiDouble (DX2# dv)) =
        let !(# a, b #) = unpackDoubleX2# dv
        in
          F# (a `plusDouble#` b)
-}
