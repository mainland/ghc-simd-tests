{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -W -Werror #-}

module Dotp.Double.Manual (
    dotp
  ) where

import Data.Primitive.Multi

import qualified Vector as V

dotp :: V.Vector Double -> V.Vector Double -> Double
dotp u v | V.length u /= V.length v =
    error "dotp: vectors of different lengths"

dotp u v =
    n `seq` k `seq` m `seq` loop 0 0
  where
    m :: Int
    m = multiplicity (undefined :: Multi Double)

    n, k :: Int
    n = min (V.length u) (V.length v)
    k = n - n `rem` m

    loop :: Multi Double -> Int -> Double
    {-# INLINE loop #-}
    loop !z i | i >= k =
        loop1 (reduce z) i

    loop !z i =
        loop (z + x*y) (i+m)
      where
        x, y :: Multi Double
        x = V.munsafeIndex u i
        y = V.munsafeIndex v i

    loop1 :: Double -> Int -> Double
    {-# INLINE loop1 #-}
    loop1 !z i | i >= n =
        z

    loop1 !z i =
        loop1 (z + x*y) (i+1)
      where
        x, y :: Double
        x = V.unsafeIndex u i
        y = V.unsafeIndex v i

    reduce :: Multi Double -> Double
    {-# INLINE reduce #-}
    reduce mf =
        multifold (+) 0 mf
