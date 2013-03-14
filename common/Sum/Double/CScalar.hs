{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Sum.Double.CScalar (
    sum
  ) where

import Prelude hiding (sum)

import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import qualified Data.Vector.Primitive as P
import Foreign.C
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr
import GHC.Ptr
import System.IO.Unsafe (unsafePerformIO)

import qualified Vector as V

foreign import ccall unsafe "c_dsum" c_dsum :: Ptr Double -> CInt -> CDouble

sum :: V.Vector Double -> Double
{-# INLINE sum #-}
sum u =
    realToFrac (c_dsum up ul)
  where
    up :: Ptr Double
    ul :: CInt
    (up, ul) = V.unsafeToPtrLen u
