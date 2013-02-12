{-# LANGUAGE BangPatterns #-}

module Sum.Double.Scalar (
    sum
  ) where

import Prelude hiding (sum)

import qualified Vector as V

sum :: V.Vector Double -> Double
sum v = V.foldl' (+) 0 v
