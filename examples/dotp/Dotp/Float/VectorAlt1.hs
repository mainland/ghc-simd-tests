{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Dotp.Float.VectorAlt1 (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U
import Util

import "vector" Data.Primitive.Multi

data Pair = Pair {-# UNPACK #-} !Float {-# UNPACK #-} !(Multi Float)

dotp :: U.Vector Float -> U.Vector Float -> Float
dotp v w =
    let Pair s ms = U.mfoldl' plus1 plusm (Pair 0 0) $ U.mzipWith (*) (*) v w
    in
      multifold (+) s ms
  where
    plusm (Pair !x (MultiFloat !mx)) (MultiFloat !my) = Pair x       (MultiFloat (mx + my))
    plus1 (Pair !x !mx)              !y               = Pair (x + y) mx
