{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module Kde.Scalar (
    kde
  ) where

import qualified Vector as V
import Common

kde :: Int
    -- ^ The number of mesh points to use in the uniform discretization
    -- of the interval @(min,max)@.  If this value is not a power of
    -- two, then it is rounded up to the next power of two.
    -> V.Vector Double -> (V.Vector Double, V.Vector Double)
kde n0 xs = kde_ n0 (lo - range / 10) (hi + range / 10) xs
  where
    (lo,hi) = minMax xs
    range   | V.length xs <= 1 = 1       -- Unreasonable guess
            | otherwise        = hi - lo

-- | Gaussian kernel density estimator for one-dimensional data, using
-- the method of Botev et al.
--
-- The result is a pair of vectors, containing:
--
-- * The coordinates of each mesh point.
--
-- * Density estimates at each mesh point.
kde_ :: Int
     -- ^ The number of mesh points to use in the uniform discretization
     -- of the interval @(min,max)@.  If this value is not a power of
     -- two, then it is rounded up to the next power of two.
     -> Double
     -- ^ Lower bound (@min@) of the mesh range.
     -> Double
     -- ^ Upper bound (@max@) of the mesh range.
     -> V.Vector Double -> (V.Vector Double, V.Vector Double)
kde_ n0 min max xs
  | V.null xs = error "Statistics.KernelDensity.kde: empty sample"
  | n0 < 1    = error "Statistics.KernelDensity.kde: invalid number of points"
  | otherwise = (mesh, density)
  where
    mesh = V.generate ni $ \z -> min + (d * fromIntegral z)
        where d = r / (n-1)
    density = V.map (/(2 * r)) . idct $ V.zipWith f a (V.enumFromTo 0 (n-1))
      where f b z = b * exp (sqr z * sqr pi * t_star * (-0.5))
    !n  = fromIntegral ni
    !ni = nextHighestPowerOfTwo n0
    !r  = max - min
    a   = dct . V.map (/ V.sum h) $ h
        where h = V.map (/ len) $ histogram_ ni min max xs
    !len    = fromIntegral (V.length xs)
    !t_star = fromRoot (0.28 * len ** (-0.4)) . ridders 1e-14 (0,0.1) $ \x ->
              x - (len * (2 * sqrt pi) * go 6 (f 7 x)) ** (-0.4)
      where
        f q t = 2 * pi ** (q*2) * V.sum (V.zipWith g iv a2v)
          where g i a2 = i ** q * a2 * exp ((-i) * sqr pi * t)
                a2v = V.map (sqr . (*0.5)) $ V.tail a
                iv = V.map sqr $ V.enumFromTo 1 (n-1)
        go s !h | s == 1    = h
                | otherwise = go (s-1) (f s time)
          where time  = (2 * const * k0 / len / h) ** (2 / (3 + 2 * s))
                const = (1 + 0.5 ** (s+0.5)) / 3
                k0    = V.product (V.enumFromThenTo 1 3 (2*s-1)) / m_sqrt_2_pi
    sqr x = x * x
