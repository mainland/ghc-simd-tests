{-# LANGUAGE BangPatterns #-}

module Kahan.SSE (
    sum
  ) where

import Prelude hiding (sum)

import Data.Primitive.Multi

import qualified Data.Vector.Unboxed as V

sum :: V.Vector Double -> Double
sum v = case V.mfoldl' kahan mkahan (State 0 0 0 0) v of
          State s c ms _ -> case multifold kahan1 (Pair s c) ms of
                              Pair s _ -> s

data State a = State !a !a !(Multi a) !(Multi a)

data Pair a = Pair !a !a

kahan1 :: Pair Double -- Current sum and current compensation
       -> Double
       -> Pair Double
kahan1 (Pair s c) x = Pair s' c'
  where
    y  = x - c
    s' = s + y
    c' = (s' - s) - y

mkahan :: State Double -- Current sum and current compensation
       -> Multi Double
       -> State Double
mkahan (State s c ms mc) x = (State s c ms' mc')
  where
    y   = x - mc
    ms' = ms + y
    mc' = (ms' - ms) - y

kahan :: State Double -- Current sum and current compensation
      -> Double
      -> State Double
kahan (State s c ms mc) x = State s' c' ms mc
  where
    y  = x - c
    s' = s + y
    c' = (s' - s) - y
