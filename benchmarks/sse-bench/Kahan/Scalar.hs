{-# LANGUAGE BangPatterns #-}

module Kahan.Scalar (
    sum
  ) where

import Prelude hiding (sum)

import qualified Data.Vector.Unboxed as V

sum :: V.Vector Double -> Double
sum v = case V.foldl' kahan (Pair 0 0) v of
          Pair s _ -> s

data Pair a = Pair !a !a

kahan :: Pair Double -- Current sum and current compensation
      -> Double
      -> Pair Double
kahan (Pair s c) x = Pair c' t
  where
    y  = x - c
    t  = s + y
    c' = (t - s) - y
