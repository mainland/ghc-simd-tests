{-# LANGUAGE BangPatterns #-}

module Sum.Int64.Scalar (
    sum
  ) where

import Data.Int
import Prelude hiding (sum)

import qualified Data.Vector.Unboxed as U

sum :: U.Vector Int64 -> Int64
sum v = U.foldl' (+) 0 v
