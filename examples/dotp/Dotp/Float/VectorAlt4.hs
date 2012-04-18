{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Dotp.Float.VectorAlt4 (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U

import "vector" Data.Primitive.Multi

dotp :: U.Vector Float -> U.Vector Float -> Float
dotp v w =
    U.mfoldlu' (+) (+) 0 $ U.mzipWith (*) (*) v w
