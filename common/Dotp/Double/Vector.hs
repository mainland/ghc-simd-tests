module Dotp.Double.Vector (
    dotp
  ) where

import qualified Vector as V

dotp :: V.Vector Double -> V.Vector Double -> Double
dotp v w = V.vfold (+) 0 (V.vzipWith (*) v w)
