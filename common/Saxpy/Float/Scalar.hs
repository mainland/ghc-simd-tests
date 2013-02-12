module Saxpy.Float.Scalar (
    saxpy
  ) where

import qualified Vector as V

saxpy :: Float -> V.Vector Float -> V.Vector Float -> V.Vector Float
saxpy a xs ys =
    V.zipWith (\x y -> a*x + y) xs ys
