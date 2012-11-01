{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Saxpy.Float.Vector (
    saxpy
  ) where

import qualified Data.Vector.Unboxed as U

import "primitive" Data.Primitive.Multi

-- The seq is very important for efficiency! It allows the evaluation of the
-- constant @a'@ to be lifted out of the inner loop.
saxpy :: Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
saxpy a xs ys =
    a' `seq` U.mzipWith (\x y -> a*x + y) (\x y -> a'*x + y) xs ys
  where
    a' :: Multi Float
    a' = multireplicate a
