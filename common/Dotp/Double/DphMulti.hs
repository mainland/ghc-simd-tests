{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise -fno-vectorisation-avoidance #-}

module Dotp.Double.DphMulti (
    dotp
  ) where

import Data.Array.Parallel
import qualified Data.Array.Parallel.Prelude.MultiDouble as D

dotp :: PArray Double -> PArray Double -> Double
{-# NOINLINE dotp #-}
dotp v w = dotp' (fromPArrayP v) (fromPArrayP w)

dotp' :: [:Double:] -> [:Double:] -> Double
dotp' v w = D.sumP (zipWithP (D.*) v w)
