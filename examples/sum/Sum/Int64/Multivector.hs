{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnboxedTuples #-}

module Sum.Int64.Multivector (
    sum
  ) where

import Data.Int
import Prelude hiding (sum)

import qualified Data.Vector.Generic.MultiStream as MS
import qualified Data.Vector.Unboxed as U

import "multivector" Data.Primitive.Multi

sum :: U.Vector Int64 -> Int64
sum v = MS.foldl' (+) (+) (multifold (+) 0) 0 v
