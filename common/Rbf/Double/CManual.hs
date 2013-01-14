{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Rbf.Double.CManual (
    rbf
  ) where

import qualified Data.Vector.Unboxed as U

import Foreign.C
import Foreign.Ptr

import Util.Unsafe

foreign import ccall "crbf" c_rbf :: CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble

rbf :: Double -> U.Vector Double -> U.Vector Double -> Double
{-# INLINE rbf #-}
rbf nu u v =
    (fromRational . toRational) (c_rbf ((fromRational . toRational) nu) up ul vp vl)
  where
    up, vp :: Ptr CDouble
    ul, vl :: CInt
    (up, ul) = unsafeDoubleUVectorToPtr u
    (vp, vl) = unsafeDoubleUVectorToPtr v
