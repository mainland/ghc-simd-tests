{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Rbf.Double.SALT (
    rbf,
    rbf_abs
  ) where

import Foreign.C
import Foreign.Ptr

import Vector

foreign import ccall unsafe "salt_rbf"
    salt_rbf :: CDouble -> Ptr Double -> CInt -> Ptr Double -> CInt -> CDouble

foreign import ccall unsafe "salt_rbf_abs"
    salt_rbf_abs :: CDouble -> Ptr Double -> CInt -> Ptr Double -> CInt -> CDouble

rbf :: Double -> Vector Double -> Vector Double -> Double
{-# INLINE rbf #-}
rbf nu u v =
    realToFrac (salt_rbf (realToFrac nu) up ul vp vl)
  where
    up, vp :: Ptr Double
    ul, vl :: CInt
    (up, ul) = unsafeToPtrLen u
    (vp, vl) = unsafeToPtrLen v

rbf_abs :: Double -> Vector Double -> Vector Double -> Double
{-# INLINE rbf_abs #-}
rbf_abs nu u v =
    realToFrac (salt_rbf_abs (realToFrac nu) up ul vp vl)
  where
    up, vp :: Ptr Double
    ul, vl :: CInt
    (up, ul) = unsafeToPtrLen u
    (vp, vl) = unsafeToPtrLen v
