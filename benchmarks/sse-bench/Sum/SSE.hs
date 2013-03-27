{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Sum.SSE (
    sum
  ) where

import Prelude hiding (sum)

import Data.Primitive.Multi

import qualified Vector as V

#if 0
data Pair a = Pair !a !(Multi a)

sum :: V.Vector Double -> Double
sum v =
    multifold (+) s ms
  where
    Pair s ms = V.mfoldl' plus1 plusm (Pair 0 0) v

    plusm (Pair x mx) my = Pair x       (mx + my)
    plus1 (Pair x mx) y  = Pair (x + y) mx
#else
sum :: V.Vector Double -> Double
sum v = V.vsum v
#endif

