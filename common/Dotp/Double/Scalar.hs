module Dotp.Double.Scalar (
    dotp
  ) where

import qualified Vector as V

dotp :: V.Vector Double -> V.Vector Double -> Double
dotp v w =
    V.sum $ V.zipWith (*) v w
