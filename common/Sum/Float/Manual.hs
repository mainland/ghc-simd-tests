{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -W -Werror #-}

module Sum.Float.Manual (
    sum
  ) where

import Prelude hiding (sum)

import Data.Primitive.Multi

import qualified Vector as V

sum :: V.Vector Float -> Float
sum u =
    n `seq` k `seq` m `seq` loop 0 0
  where
    m :: Int
    m = multiplicity (undefined :: Multi Float)

    n, k :: Int
    n = V.length u
    k = n - n `rem` m

    loop :: Multi Float -> Int -> Float
    {-# INLINE loop #-}
    loop !z i | i >= k =
        loop1 (reduce z) i

    loop !z i =
        loop (z + x) (i+m)
      where
        x :: Multi Float
        x = V.munsafeIndex u i

    loop1 :: Float -> Int -> Float
    {-# INLINE loop1 #-}
    loop1 !z i | i >= n =
        z

    loop1 !z i =
        loop1 (z + x) (i+1)
      where
        x :: Float
        x = V.unsafeIndex u i

    reduce :: Multi Float -> Float
    {-# INLINE reduce #-}
    reduce mf =
        multifold (+) 0 mf
{-
    reduce (MultiFloat (FX4# fv)) =
        let !(# a, b, c, d #) = unpackFloatX4# fv
        in
          F# (a `plusFloat#` b `plusFloat#` c `plusFloat#` d)
-}
