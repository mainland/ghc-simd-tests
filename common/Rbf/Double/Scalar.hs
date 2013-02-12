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

import qualified Vector as V

rbf :: Double -> V.Vector Double -> V.Vector Double -> Double
rbf nu v w =
    exp (-nu * V.sum (V.zipWith norm v w))
  where
    square x = x * x
    norm x y = square (x-y)
