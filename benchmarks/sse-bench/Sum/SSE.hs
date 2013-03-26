{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Sum.SSE (
    sum
  ) where

import Prelude hiding (sum)

import Data.Primitive.Multi

import qualified Data.Vector.Unboxed as V

data State a = State !a !(Multi a)

sum :: V.Vector Double -> Double
sum v =
    multifold (+) s ms
  where
    State s ms = V.mfoldl' plus1 plusm (State 0 0) v

    plusm (State x mx) my = State x       (mx + my)
    plus1 (State x mx) y  = State (x + y) mx
