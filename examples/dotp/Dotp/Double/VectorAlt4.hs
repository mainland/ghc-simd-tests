{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Dotp.Double.VectorAlt4 (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U

import "vector" Data.Primitive.Multi

dotp :: U.Vector Double -> U.Vector Double -> Double
dotp v w =
    U.mfoldlu' (+) (+) 0 $ U.mzipWith (*) (*) v w
