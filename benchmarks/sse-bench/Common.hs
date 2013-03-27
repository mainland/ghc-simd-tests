{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}

module Common  where

#include "MachDeps.h"

import Control.Applicative
import Control.Monad            (MonadPlus(..), ap, when)
import Control.Monad.ST         (ST, runST)
import Data.Bits                ((.&.), (.|.), shiftL, shiftR)
import Data.Complex             (Complex(..), conjugate, realPart)
import Data.Int                 (Int64)
import Data.Primitive.ByteArray (newByteArray, readByteArray, writeByteArray)
import Data.Primitive.Multi

import GHC.Prim

import qualified Vector as V

import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed         as U

data MM = MM {-# UNPACK #-} !Double {-# UNPACK #-} !Double

-- | Compute the minimum and maximum of a vector in one pass.
minMax :: V.Vector Double -> (Double, Double)
minMax = fini . V.foldl' go (MM (1/0) (-1/0))
  where
    go (MM lo hi) k = MM (min lo k) (max hi k)
    fini (MM lo hi) = (lo, hi)
{-# INLINE minMax #-}

-- | Efficiently compute the next highest power of two for a
-- non-negative integer.  If the given value is already a power of
-- two, it is returned unchanged.  If negative, zero is returned.
nextHighestPowerOfTwo :: Int -> Int
nextHighestPowerOfTwo n = o + 1
    where m = n - 1
          o = m
              .|. (m `shiftR` 1)
              .|. (m `shiftR` 2)
              .|. (m `shiftR` 4)
              .|. (m `shiftR` 8)
              .|. (m `shiftR` 16)
#if WORD_SIZE_IN_BITS == 64
              .|. (m `shiftR` 32)
#endif



type CD = Complex Double

-- | Discrete cosine transform (DCT-II).
dct :: U.Vector Double -> U.Vector Double
dct = dctWorker . G.map (:+0)

-- | Discrete cosine transform (DCT-II). Only real part of vector is
--   transformed, imaginary part is ignored.
dct_ :: U.Vector CD -> U.Vector Double
dct_ = dctWorker . G.map (\(i :+ _) -> i :+ 0)

dctWorker :: U.Vector CD -> U.Vector Double
dctWorker xs
  = G.map realPart $ G.zipWith (*) weights (fft interleaved)
  where
    interleaved = G.backpermute xs $ G.enumFromThenTo 0 2 (len-2) G.++
                                     G.enumFromThenTo (len-1) (len-3) 1
    weights = G.cons 2 . G.generate (len-1) $ \x ->
              2 * exp ((0:+(-1))*fi (x+1)*pi/(2*n))
      where n = fi len
    len = G.length xs



-- | Inverse discrete cosine transform (DCT-III). It's inverse of
-- 'dct' only up to scale parameter:
--
-- > (idct . dct) x = (* lenngth x)
idct :: U.Vector Double -> U.Vector Double
idct = idctWorker . G.map (:+0)

-- | Inverse discrete cosine transform (DCT-III). Only real part of vector is
--   transformed, imaginary part is ignored.
idct_ :: U.Vector CD -> U.Vector Double
idct_ = idctWorker . G.map (\(i :+ _) -> i :+ 0)

idctWorker :: U.Vector CD -> U.Vector Double
idctWorker xs = G.generate len interleave
  where
    interleave z | even z    = vals `G.unsafeIndex` halve z
                 | otherwise = vals `G.unsafeIndex` (len - halve z - 1)
    vals = G.map realPart . ifft $ G.zipWith (*) weights xs
    weights
      = G.cons n
      $ G.generate (len - 1) $ \x -> 2 * n * exp ((0:+1) * fi (x+1) * pi/(2*n))
      where n = fi len
    len = G.length xs


-- | Inverse fast Fourier transform.
ifft :: U.Vector CD -> U.Vector CD
ifft xs = G.map ((/fi (G.length xs)) . conjugate) . fft . G.map conjugate $ xs

-- | Radix-2 decimation-in-time fast Fourier transform.
fft :: U.Vector CD -> U.Vector CD
fft v = G.create $ do
          mv <- G.thaw v
          mfft mv
          return mv

mfft :: (GM.MVector v CD) => v s CD -> ST s ()
mfft vec
    | 1 `shiftL` m /= len = error "Statistics.Transform.fft: bad vector size"
    | otherwise           = bitReverse 0 0
 where
  bitReverse i j | i == len-1 = stage 0 1
                 | otherwise  = do
    when (i < j) $ GM.swap vec i j
    let inner k l | k <= l    = inner (k `shiftR` 1) (l-k)
                  | otherwise = bitReverse (i+1) (l+k)
    inner (len `shiftR` 1) j
  stage l !l1 | l == m    = return ()
              | otherwise = do
    let !l2 = l1 `shiftL` 1
        !e  = -6.283185307179586/fromIntegral l2
        flight j !a | j == l1   = stage (l+1) l2
                    | otherwise = do
          let butterfly i | i >= len  = flight (j+1) (a+e)
                          | otherwise = do
                let i1 = i + l1
                xi1 :+ yi1 <- GM.read vec i1
                let !c = cos a
                    !s = sin a
                    d  = (c*xi1 - s*yi1) :+ (s*xi1 + c*yi1)
                ci <- GM.read vec i
                GM.write vec i1 (ci - d)
                GM.write vec i (ci + d)
                butterfly (i+l2)
          butterfly j
    flight 0 0
  len = GM.length vec
  m   = log2 len

fi :: Int -> CD
fi = fromIntegral

halve :: Int -> Int
halve = (`shiftR` 1)

-- | /O(log n)/ Compute the logarithm in base 2 of the given value.
log2 :: Int -> Int
log2 v0
    | v0 <= 0   = modErr $ "log2: negative input, got " ++ show v0
    | otherwise = go 5 0 v0
  where
    go !i !r !v | i == -1        = r
                | v .&. b i /= 0 = let si = U.unsafeIndex sv i
                                   in go (i-1) (r .|. si) (v `shiftR` si)
                | otherwise      = go (i-1) r v
    b = U.unsafeIndex bv
    !bv = U.fromList [0x2, 0xc, 0xf0, 0xff00, 0xffff0000, 0xffffffff00000000]
    !sv = U.fromList [1,2,4,8,16,32]

modErr :: String -> a
modErr msg = error $ "Numeric.SpecFunctions." ++ msg

-- | @sqrt (2 * pi)@
m_sqrt_2_pi :: Double
m_sqrt_2_pi = 2.5066282746310005024157652848110452530069867406099383166299
{-# INLINE m_sqrt_2_pi #-}


-- | The result of searching for a root of a mathematical function.
data Root a = NotBracketed
            -- ^ The function does not have opposite signs when
            -- evaluated at the lower and upper bounds of the search.
            | SearchFailed
            -- ^ The search failed to converge to within the given
            -- error tolerance after the given number of iterations.
            | Root a
            -- ^ A root was successfully found.
              deriving (Eq, Read, Show)

instance Functor Root where
    fmap _ NotBracketed = NotBracketed
    fmap _ SearchFailed = SearchFailed
    fmap f (Root a)     = Root (f a)

instance Monad Root where
    NotBracketed >>= _ = NotBracketed
    SearchFailed >>= _ = SearchFailed
    Root a       >>= m = m a

    return = Root

instance MonadPlus Root where
    mzero = SearchFailed

    r@(Root _) `mplus` _ = r
    _          `mplus` p = p

instance Applicative Root where
    pure  = Root
    (<*>) = ap

instance Alternative Root where
    empty = SearchFailed

    r@(Root _) <|> _ = r
    _          <|> p = p

-- | Returns either the result of a search for a root, or the default
-- value if the search failed.
fromRoot :: a                   -- ^ Default value.
         -> Root a              -- ^ Result of search for a root.
         -> a
fromRoot _ (Root a) = a
fromRoot a _        = a


-- | Use the method of Ridders to compute a root of a function.
--
-- The function must have opposite signs when evaluated at the lower
-- and upper bounds of the search (i.e. the root must be bracketed).
ridders :: Double               -- ^ Absolute error tolerance.
        -> (Double,Double)      -- ^ Lower and upper bounds for the search.
        -> (Double -> Double)   -- ^ Function to find the roots of.
        -> Root Double
ridders tol (lo,hi) f
    | flo == 0    = Root lo
    | fhi == 0    = Root hi
    | flo*fhi > 0 = NotBracketed -- root is not bracketed
    | otherwise   = go lo flo hi fhi 0
  where
    go !a !fa !b !fb !i
        -- Root is bracketed within 1 ulp. No improvement could be made
        | within 1 a b       = Root a
        -- Root is found. Check that f(m) == 0 is nessesary to ensure
        -- that root is never passed to 'go'
        | fm == 0            = Root m
        | fn == 0            = Root n
        | d < tol            = Root n
        -- Too many iterations performed. Fail
        | i >= (100 :: Int)  = SearchFailed
        -- Ridder's approximation coincide with one of old
        -- bounds. Revert to bisection
        | n == a || n == b   = case () of
          _| fm*fa < 0 -> go a fa m fm (i+1)
           | otherwise -> go m fm b fb (i+1)
        -- Proceed as usual
        | fn*fm < 0          = go n fn m fm (i+1)
        | fn*fa < 0          = go a fa n fn (i+1)
        | otherwise          = go n fn b fb (i+1)
      where
        d    = abs (b - a)
        dm   = (b - a) * 0.5
        !m   = a + dm
        !fm  = f m
        !dn  = signum (fb - fa) * dm * fm / sqrt(fm*fm - fa*fb)
        !n   = m - signum dn * min (abs dn) (abs dm - 0.5 * tol)
        !fn  = f n
    !flo = f lo
    !fhi = f hi

-- | Compare two 'Double' values for approximate equality, using
-- Dawson's method.
--
-- The required accuracy is specified in ULPs (units of least
-- precision).  If the two numbers differ by the given number of ULPs
-- or less, this function returns @True@.
within :: Int                   -- ^ Number of ULPs of accuracy desired.
       -> Double -> Double -> Bool
within ulps a b = runST $ do
  buf <- newByteArray 8
  ai0 <- writeByteArray buf 0 a >> readByteArray buf 0
  bi0 <- writeByteArray buf 0 b >> readByteArray buf 0
  let big  = 0x8000000000000000 :: Int64
      ai | ai0 < 0   = big - ai0
         | otherwise = ai0
      bi | bi0 < 0   = big - bi0
         | otherwise = bi0
  return $ abs (ai - bi) <= fromIntegral ulps


-- | /O(n)/ Compute a histogram over a data set.
--
-- Interval (bin) sizes are uniform, based on the supplied upper
-- and lower bounds.
histogram_ :: (Num b, RealFrac a, G.Vector v0 a, G.Vector v1 b) =>
              Int
           -- ^ Number of bins.  This value must be positive.  A zero
           -- or negative value will cause an error.
           -> a
           -- ^ Lower bound on interval range.  Sample data less than
           -- this will cause an error.
           -> a
           -- ^ Upper bound on interval range.  This value must not be
           -- less than the lower bound.  Sample data that falls above
           -- the upper bound will cause an error.
           -> v0 a
           -- ^ Sample data.
           -> v1 b
histogram_ numBins lo hi xs0 = G.create (GM.replicate numBins 0 >>= bin xs0)
  where
    bin xs bins = go 0
     where
       go i | i >= len = return bins
            | otherwise = do
         let x = xs `G.unsafeIndex` i
             b = truncate $ (x - lo) / d
         GM.write bins b . (+1) =<< GM.read bins b
         go (i+1)
       len = G.length xs
       d = ((hi - lo) * (1 + realToFrac m_epsilon)) / fromIntegral numBins
{-# INLINE histogram_ #-}

-- | The smallest 'Double' &#949; such that 1 + &#949; &#8800; 1.
m_epsilon :: Double
m_epsilon = encodeFloat (signif+1) expo - 1.0
    where (signif,expo) = decodeFloat (1.0::Double)

instance Fractional (Multi Double) where
    MultiDouble (DX2# x#) / MultiDouble (DX2# y#) = MultiDouble (DX2# (divideDoubleX2# x# y#))

    fromRational x = multireplicate (fromRational x)

-- Minimal complete definition:
--      'pi', 'exp', 'log', 'sin', 'cos', 'sinh', 'cosh',
--      'asin', 'acos', 'atan', 'asinh', 'acosh' and 'atanh'
instance Floating (Multi Double) where
    pi    = multireplicate pi
    exp   = multimap exp
    log   = multimap log
    sin   = multimap sin
    cos   = multimap cos
    sinh  = multimap sinh
    cosh  = multimap cosh
    asin  = multimap asin
    acos  = multimap acos
    atan  = multimap atan
    asinh = multimap asinh
    acosh = multimap acosh
    atanh = multimap atanh
