{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Dotp.Float.VectorAlt2 (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U
import Util

import "primitive" Data.Primitive.Multi

data Pair = Pair {-# UNPACK #-} !Float {-# UNPACK #-} !FloatX4

dotp :: U.Vector Float -> U.Vector Float -> Float
dotp v w =
    let Pair s ms = U.mfoldl' plus1 plusm (Pair 0 0) $ U.mzipWith (*) (*) v w
    in
      multifold (+) s (MultiFloat ms)
  where
    plusm (Pair !x !mx)  (MultiFloat !my) = Pair x       (mx + my)
    plus1 (Pair !x !mx)               !y  = Pair (x + y) mx
