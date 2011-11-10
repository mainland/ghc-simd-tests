module Single (
    dotp
  ) where

import qualified Data.Vector.Unboxed as U

dotp :: U.Vector Float -> U.Vector Float -> Float
dotp v w =
    U.sum $ U.zipWith (*) v w
