{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module MultiLazy (
    sumFrom1To
  ) where

import qualified Data.Vector.Generic.MultiStream as MS
import qualified Data.Vector.Unboxed as U

import Data.Primitive.Multi
import GHC.Int
import GHC.Prim

sumFrom1To :: Int32 -> Int32
sumFrom1To mx = MS.foldl' (+) (+) red 0 v
  where
    v :: U.Vector Int32
    v = U.enumFromTo 1 mx

    red :: Multi Int32 -> Int32
    red (MultiInt32 (I32X4# iv)) =
        let !(# a, b, c, d #) = unpackInt32X4# iv
        in
          I32# (a +# b +# c +# d)
