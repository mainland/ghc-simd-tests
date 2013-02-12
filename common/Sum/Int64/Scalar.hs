{-# LANGUAGE BangPatterns #-}

module Sum.Int64.Scalar (
    sum
  ) where

import Data.Int
import Prelude hiding (sum)

import qualified Vector as V

sum :: V.Vector Int64 -> Int64
sum v = V.foldl' (+) 0 v
