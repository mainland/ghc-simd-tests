{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module MultiStrict (
    sumFrom1To
  ) where

import qualified Data.Vector.Generic.MultiStream as MS
import qualified Data.Vector.Unboxed as U

import Data.Primitive.Multi
import GHC.Int
import GHC.Prim

sumFrom1To :: U.Vector Int32 -> Int32
sumFrom1To v = MS.foldl' (+) (+) red 0 v
  where
    red :: Multi Int32 -> Int32
    red (MultiInt32 (I32X4# iv)) =
        let !(# a, b, c, d #) = unpackInt32X4# iv
        in
          I32# (a +# b +# c +# d)
