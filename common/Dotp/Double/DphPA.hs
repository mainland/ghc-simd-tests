{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}

module Dotp.Double.DphPA (
    dotp
  ) where

import Data.Array.Parallel
import Data.Array.Parallel.Prelude
import qualified Data.Array.Parallel.Prelude.Double as D

import qualified Prelude

dotp :: PArray Double -> PArray Double -> Double
{-# NOINLINE dotp #-}
dotp v w = dotp' (fromPArrayP v) (fromPArrayP w)

dotp' :: [:Double:] -> [:Double:] -> Double
dotp' xs ys = D.sumP [:x D.* y | x <- xs | y <- ys:]
