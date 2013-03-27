module Dotp.SSE (
    dotp
  ) where

import qualified Vector as V

dotp :: V.Vector Double -> V.Vector Double -> Double
dotp v w = V.vsum (V.vzipWith (*) v w)
