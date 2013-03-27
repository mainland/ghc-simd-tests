{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module VarianceUnbiased.SSE (
    varianceUnbiased
  ) where

import Data.Primitive.Multi

import qualified Vector as V
import Common

-- | Unbiased estimate of a sample's variance.  Also known as the
-- sample variance, where the denominator is /n/-1.
varianceUnbiased :: V.Vector Double -> Double
varianceUnbiased samp
    | n > 1     = robustSumVar (mean samp) samp / fromIntegral (n-1)
    | otherwise = 0
    where
      n = V.length samp
{-# INLINE varianceUnbiased #-}

robustSumVar :: Double -> V.Vector Double -> Double
robustSumVar m samp = V.vsum . V.mmap (square . subtract m) (square . subtract mm) $ samp
  where square x = x * x
        !mm = multireplicate m
{-# INLINE robustSumVar #-}

data T = T {-# UNPACK #-}!Double {-# UNPACK #-}!(Multi Double) {-# UNPACK #-}!Int

-- | /O(n)/ Arithmetic mean.  This uses Welford's algorithm to provide
-- numerical stability, using a single pass over the sample data.
mean :: V.Vector Double -> Double
mean = fini . V.mfoldl' go mgo (T 0 0 0)
  where
    fini (T a ma _) = multifold (+) a ma

    mgo (T m mm n) x = T m mm' n'
        where mm' = mm + (x - mm) / multireplicate (fromIntegral n')
              n'  = n + 1

    go (T m mm n) x = T m' mm n'
        where m' = m + (x - m) / fromIntegral n'
              n' = n + 1
{-# INLINE mean #-}
