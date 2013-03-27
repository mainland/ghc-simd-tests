{-# LANGUAGE BangPatterns #-}

module Saxpy.SSE (
    saxpy
  ) where

import qualified Vector as V

import Data.Primitive.Multi

saxpy :: Double -> V.Vector Double -> V.Vector Double -> V.Vector Double
saxpy alpha x y =
    V.vzipWith (+) (V.mmap (* alpha) (* malpha) x) y
  where
    malpha :: Multi Double
    !malpha = multireplicate alpha
