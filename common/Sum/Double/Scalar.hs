{-# LANGUAGE BangPatterns #-}

module Sum.Double.Scalar (
    sum
  ) where

import Prelude hiding (sum)

import qualified Data.Vector.Unboxed as U

sum :: U.Vector Double -> Double
sum v = U.foldl' (+) 0 v
