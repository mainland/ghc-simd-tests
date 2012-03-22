{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -W -Werror #-}

module Dotp.Float.Manual (
    dotp
  ) where

#if __GLASGOW_HASKELL__ >= 750
import "vector" Data.Primitive.Multi
#else /* __GLASGOW_HASKELL__ < 750 */
import "multivector" Data.Primitive.Multi
import Data.Vector.Unboxed.Packed
#endif /* __GLASGOW_HASKELL__ < 750 */
import qualified Data.Vector.Unboxed as U

dotp :: U.Vector Float -> U.Vector Float -> Float
{-# NOINLINE dotp #-}
dotp u v | U.length u /= U.length v =
    error "dotp: vectors of different lengths"

dotp u v =
    n `seq` k `seq` m `seq` loop 0 0
  where
    m :: Int
    m = multiplicity (undefined :: Multi Float)

    n, k :: Int
    n = min (U.length u) (U.length v)
    k = n - n `rem` m

    loop :: Multi Float -> Int -> Float
    {-# INLINE loop #-}
    loop !z i | i >= k =
        loop1 (reduce z) i

    loop !z i =
        loop (z + x*y) (i+4)
      where
        x, y :: Multi Float
#if __GLASGOW_HASKELL__ >= 750
        x = U.munsafeIndex u i
        y = U.munsafeIndex v i
#else /* __GLASGOW_HASKELL__ < 750 */
        x = unsafeIndexAsMulti u i
        y = unsafeIndexAsMulti v i
#endif /* __GLASGOW_HASKELL__ < 750 */

    loop1 :: Float -> Int -> Float
    {-# INLINE loop1 #-}
    loop1 !z i | i >= n =
        z

    loop1 !z i =
        loop1 (z + x*y) (i+1)
      where
        x, y :: Float
        x = U.unsafeIndex u i
        y = U.unsafeIndex v i

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