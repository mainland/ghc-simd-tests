{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Sum.Float.CScalar (
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

import Util

foreign import ccall "csum" c_sum :: Ptr CFloat -> CInt -> CFloat

sum :: U.Vector Float -> Float
{-# INLINE sum #-}
sum u =
    (fromRational . toRational) (c_sum up ul)
  where
    up :: Ptr CFloat
    ul :: CInt
    (up, ul) = unsafeFloatUVectorToPtr u
