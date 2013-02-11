{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Rbf.Double.VectorAlt2 (
    rbf
  ) where

import Data.Primitive.Multi

import Vector

rbf :: Double -> Vector Double -> Vector Double -> Double
rbf nu x y =
    exp (-nu * norm2 z)
  where
    z        = vzipWith (-) x y
    dot u v  = vsum (vzipWith (*) u v)
    norm2 u  = dot u u
