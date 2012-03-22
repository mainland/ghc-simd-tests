{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Dotp.Float.VectorAlt3 (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U
import OldFold
import Util

import "vector" Data.Primitive.Multi

dotp :: U.Vector Float -> U.Vector Float -> Float
dotp v w =
    u_mfoldl' (+) (+) 0 (multifold (+) 0) $ U.mzipWith (*) (*) v w
