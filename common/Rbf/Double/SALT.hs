{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Rbf.Double.SALT (
    rbf
  ) where

import Foreign.C
import Foreign.Ptr

import Vector

foreign import ccall "salt_rbf" salt_rbf :: CDouble -> Ptr Double -> CInt -> Ptr Double -> CInt -> CDouble

rbf :: Double -> Vector Double -> Vector Double -> Double
{-# INLINE rbf #-}
rbf nu u v =
    (fromRational . toRational)
      (salt_rbf ((fromRational . toRational) nu) up ul vp vl)
  where
    up, vp :: Ptr Double
    ul, vl :: CInt
    (up, ul) = unsafeToPtrLen u
    (vp, vl) = unsafeToPtrLen v
