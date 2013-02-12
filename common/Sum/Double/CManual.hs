{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Sum.Double.CManual (
    sum
  ) where

import Prelude hiding (sum)

import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import Foreign.C
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Ptr
import GHC.Ptr
import System.IO.Unsafe (unsafePerformIO)

import qualified Vector as V

foreign import ccall "c_vecdsum" c_vecdsum :: Ptr Double -> CInt -> CDouble

sum :: V.Vector Double -> Double
{-# INLINE sum #-}
sum u =
    (fromRational . toRational) (c_vecdsum up ul)
  where
    up :: Ptr Double
    ul :: CInt
    (up, ul) = V.unsafeToPtrLen u
