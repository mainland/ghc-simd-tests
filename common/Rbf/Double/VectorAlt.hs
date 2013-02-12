{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Rbf.Double.VectorAlt (
    rbf
  ) where

import Data.Primitive.Multi
import qualified Data.Vector.Generic as G

import Vector

rbf :: Double -> Vector Double -> Vector Double -> Double
rbf nu x y =
    exp (-nu * vsum (vmap square z))
  where
    z         = vzipWith (-) x y
    square x  = x * x
