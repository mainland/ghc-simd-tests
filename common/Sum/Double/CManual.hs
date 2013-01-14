{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Sum.Double.CManual (
    sum
  ) where

import Prelude hiding (sum)

import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U

import Foreign.C
import Foreign.Ptr

import GHC.Ptr

import System.IO.Unsafe (unsafePerformIO)

import Util.Unsafe

foreign import ccall "cdsum" c_dsum :: Ptr CDouble -> CInt -> CDouble

sum :: U.Vector Double -> Double
{-# INLINE sum #-}
sum u =
    (fromRational . toRational) (c_dsum up ul)
  where
    up :: Ptr CDouble
    ul :: CInt
    (up, ul) = unsafeDoubleUVectorToPtr u
