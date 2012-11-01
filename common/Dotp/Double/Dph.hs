{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS -fvectorise #-}

module Dotp.Double.Dph (
    dotp
  ) where

import Data.Array.Parallel
import qualified Data.Array.Parallel.Prelude.Double as D

dotp :: PArray Double -> PArray Double -> Double
{-# NOINLINE dotp #-}
dotp v w = dotp' (fromPArrayP v) (fromPArrayP w)

dotp' :: [:Double:] -> [:Double:] -> Double
dotp' v w = D.sumP (zipWithP (D.*) v w)
