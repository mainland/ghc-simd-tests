module Saxpy.Float.Vector (
    saxpy
  ) where

import Data.Primitive.Multi

import qualified Vector as V

-- The seq is very important for efficiency! It allows the evaluation of the
-- constant @a'@ to be lifted out of the inner loop.
saxpy :: Float -> V.Vector Float -> V.Vector Float -> V.Vector Float
saxpy a xs ys =
    a' `seq` V.mzipWith (\x y -> a*x + y) (\x y -> a'*x + y) xs ys
  where
    a' :: Multi Float
    a' = multireplicate a
