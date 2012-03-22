{-# LANGUAGE BangPatterns #-}

module Sum.Float.Scalar (
    sum
  ) where

import Prelude hiding (sum)

import qualified Data.Vector.Unboxed as U

sum :: U.Vector Float -> Float
sum v = U.foldl' (+) 0 v
