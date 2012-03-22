module Dotp.Double.Scalar (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U

dotp :: U.Vector Double -> U.Vector Double -> Double
dotp v w =
    U.sum $ U.zipWith (*) v w
