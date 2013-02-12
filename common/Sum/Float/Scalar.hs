{-# LANGUAGE BangPatterns #-}

module Sum.Float.Scalar (
    sum
  ) where

import Prelude hiding (sum)

import qualified Vector as V

sum :: V.Vector Float -> Float
sum v = V.foldl' (+) 0 v
