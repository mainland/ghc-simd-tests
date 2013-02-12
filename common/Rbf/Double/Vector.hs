{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Rbf.Double.Vector (
    rbf
  ) where

import Data.Primitive.Multi
import qualified Data.Vector.Generic as G

import Vector as V

rbf :: Double -> Vector Double -> Vector Double -> Double
rbf nu v w =
    exp (-nu * vsum (vzipWith norm v w))
  where
    square x = x * x
    norm x y = square (x-y)
