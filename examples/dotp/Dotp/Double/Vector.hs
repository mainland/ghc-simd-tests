{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Dotp.Double.Vector (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U

import "primitive" Data.Primitive.Multi

dotp :: U.Vector Double -> U.Vector Double -> Double
dotp v w =
    multifold (+) s ms
  where
    (s, ms) = U.mfoldl' plus1 plusm (0, 0) $ U.mzipWith (*) (*) v w
    plusm (!x, !mx) !my = (x, mx + my)
    plus1 (!x, !mx) !y  = (x + y, mx)
