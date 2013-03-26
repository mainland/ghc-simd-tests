{-# LANGUAGE BangPatterns #-}

module Sum.Scalar (
    sum
  ) where

import Prelude hiding (sum)

import qualified Data.Vector.Unboxed as V

sum :: V.Vector Double -> Double
sum v = V.foldl' (+) 0 v
