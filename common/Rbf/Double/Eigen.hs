{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Rbf.Double.Eigen (
    rbf,
    rbf2
  ) where

import Foreign.C
import Foreign.Ptr

import Vector

foreign import ccall unsafe "eigen_rbf"  eigen_rbf :: CDouble -> Ptr Double -> CInt -> Ptr Double -> CInt -> CDouble
foreign import ccall unsafe "eigen_rbf2" eigen_rbf2 :: CDouble -> Ptr Double -> CInt -> Ptr Double -> CInt -> CDouble

rbf :: Double -> Vector Double -> Vector Double -> Double
{-# INLINE rbf #-}
rbf nu u v =
    realToFrac (eigen_rbf (realToFrac nu) up ul vp vl)
  where
    up, vp :: Ptr Double
    ul, vl :: CInt
    (up, ul) = unsafeToPtrLen u
    (vp, vl) = unsafeToPtrLen v

rbf2 :: Double -> Vector Double -> Vector Double -> Double
{-# INLINE rbf2 #-}
rbf2 nu u v =
    realToFrac (eigen_rbf2 (realToFrac nu) up ul vp vl)
  where
    up, vp :: Ptr Double
    ul, vl :: CInt
    (up, ul) = unsafeToPtrLen u
    (vp, vl) = unsafeToPtrLen v
