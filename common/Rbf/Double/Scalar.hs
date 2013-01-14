{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Rbf.Double.Scalar (
    rbf
  ) where

import Data.Primitive.Multi
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed as U

rbf :: Double -> U.Vector Double -> U.Vector Double -> Double
rbf nu v w =
    exp (-nu * U.sum (U.zipWith norm v w))
  where
    square x = x * x
    norm x y = square (x-y)
