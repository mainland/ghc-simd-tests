{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}

module VarianceUnbiased.Scalar (
    varianceUnbiased
  ) where

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
robustSumVar m samp = V.sum . V.map (square . subtract m) $ samp
  where square x = x * x
{-# INLINE robustSumVar #-}

data T = T {-# UNPACK #-}!Double {-# UNPACK #-}!Int

-- | /O(n)/ Arithmetic mean.  This uses Welford's algorithm to provide
-- numerical stability, using a single pass over the sample data.
mean :: V.Vector Double -> Double
mean = fini . V.foldl' go (T 0 0)
  where
    fini (T a _) = a
    go (T m n) x = T m' n'
        where m' = m + (x - m) / fromIntegral n'
              n' = n + 1
{-# INLINE mean #-}
