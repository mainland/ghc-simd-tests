module Single (
    saxpy
  ) where

import qualified Data.Vector.Unboxed as U

saxpy :: Float -> U.Vector Float -> U.Vector Float -> U.Vector Float
saxpy a xs ys =
    U.zipWith (\x y -> a*x + y) xs ys
