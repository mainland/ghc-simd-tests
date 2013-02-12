{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Sum.Double.Vector (
    sum
  ) where

import Prelude hiding (sum)

import Data.Primitive.Multi

import qualified Vector as V

sum :: V.Vector Double -> Double
sum v =
    multifold (+) s ms
  where
    (s, ms) = V.mfoldl' plus1 plusm (0, 0) v
    plusm (x, mx) my = (x, mx + my)
    plus1 (x, mx) y  = (x + y, mx)
