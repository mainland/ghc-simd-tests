module Saxpy.Scalar (
    saxpy
  ) where

import qualified Vector as V

saxpy :: Double -> V.Vector Double -> V.Vector Double -> V.Vector Double
saxpy alpha x y  =
    V.zipWith (+) (V.map (* alpha) x) y
