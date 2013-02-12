module Dotp.Float.Scalar (
    dotp
  ) where

import qualified Vector as V

dotp :: V.Vector Float -> V.Vector Float -> Float
dotp v w =
    V.sum $ V.zipWith (*) v w
