{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}

module Saxpy.Float.Vector (
    saxpy
  ) where

import Data.Primitive.Multi
import qualified Data.Vector.Unboxed as U

-- The seq is very important for efficiency! It allows the evaluation of the
-- constant @a'@ to be lifted out of the inner loop.
saxpy :: Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
saxpy a xs ys =
    a' `seq` U.mzipWith (\x y -> a*x + y) (\x y -> a'*x + y) xs ys
  where
    a' :: Multi Float
    a' = multireplicate a
