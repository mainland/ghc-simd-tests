{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Rbf.Double.CManualIntermediate (
    rbf
  ) where

import qualified Data.Vector.Unboxed as U

import Foreign.C
import Foreign.Ptr

import Util.Unsafe

foreign import ccall "crbf_intermediate" c_rbf_intermediate :: CDouble -> Ptr CDouble -> CInt -> Ptr CDouble -> CInt -> CDouble

rbf :: Double -> U.Vector Double -> U.Vector Double -> Double
{-# INLINE rbf #-}
rbf nu u v =
    (fromRational . toRational) (c_rbf_intermediate ((fromRational . toRational) nu) up ul vp vl)
  where
    up, vp :: Ptr CDouble
    ul, vl :: CInt
    (up, ul) = unsafeDoubleUVectorToPtr u
    (vp, vl) = unsafeDoubleUVectorToPtr v
