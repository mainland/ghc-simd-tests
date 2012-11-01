-- |
-- Module      : Util.Unsafe
-- Copyright   : (c) Geoffrey Mainland 2011-2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Util.Unsafe (
    unsafeFloatUVectorToPtr,
    unsafeDoubleUVectorToPtr
  ) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import Foreign.C
import Foreign.Ptr
import GHC.Ptr
--import System.CPUTime (getCPUTime)
import System.Random (Random, newStdGen, randomRs)
import Text.Printf

unsafeFloatUVectorToPtr :: U.Vector Float -> (Ptr CFloat, CInt)
{-# INLINE unsafeFloatUVectorToPtr #-}
unsafeFloatUVectorToPtr (U.V_Float (P.Vector off len arr)) =
    (p, fromIntegral (fromIntegral (len - off)))
  where
    p :: Ptr CFloat
    p = case byteArrayContents arr `plusAddr` off*sz of
          Addr a -> Ptr a

    sz :: Int
    sz = sizeOf (undefined :: Float)

unsafeDoubleUVectorToPtr :: U.Vector Double -> (Ptr CDouble, CInt)
{-# INLINE unsafeDoubleUVectorToPtr #-}
unsafeDoubleUVectorToPtr (U.V_Double (P.Vector off len arr)) =
    (p, fromIntegral (fromIntegral (len - off)))
  where
    p :: Ptr CDouble
    p = case byteArrayContents arr `plusAddr` off*sz of
          Addr a -> Ptr a

    sz :: Int
    sz = sizeOf (undefined :: Double)
