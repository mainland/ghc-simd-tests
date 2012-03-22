{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE UnboxedTuples #-}

module Sum.Float.Multivector (
    sum
  ) where

import Prelude hiding (sum)

import qualified Data.Vector.Generic.MultiStream as MS
import qualified Data.Vector.Unboxed as U

import "multivector" Data.Primitive.Multi

sum :: U.Vector Float -> Float
sum v = MS.foldl' (+) (+) (multifold (+) 0) 0 v
