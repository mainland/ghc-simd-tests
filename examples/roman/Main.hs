{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad (liftM)
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Primitive
import Data.Primitive.Multi
import Foreign.ForeignPtr (ForeignPtr,
                           newForeignPtr,
                           newForeignPtr_,
                           withForeignPtr)
import Foreign.Marshal.Alloc (finalizerFree)
import Foreign.Marshal.Array (mallocArray,
                              newArray,
                              peekArray,
                              withArray)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peekElemOff, pokeElemOff)

main :: IO ()
main = do
    testForeignPtr 10 f1_scalar
    testForeignPtr 10 f1
    testByteArray 13 f2_scalar
    testByteArray 13 f2
  where
    testForeignPtr :: Int
                   -> (Int
                       -> ForeignPtr Float
                       -> ForeignPtr Float
                       -> IO (ForeignPtr Float))
                   -> IO ()
    testForeignPtr n f =
        withArray [1..fromIntegral n] $ \aptr -> do
        withArray [1..fromIntegral n] $ \bptr -> do
        afptr <- newForeignPtr_ aptr
        bfptr <- newForeignPtr_ bptr
        cfptr <- f n afptr bfptr
        withForeignPtr cfptr $ \cptr -> do
        c <- peekArray n cptr
        print c
        print $ zipWith  (\a b -> a^2 + b^2)
                         [1..fromIntegral n::Float]
                         [1..fromIntegral n::Float]
    testByteArray :: Int
                   -> (forall s . Int
                       -> ByteArray
                       -> ByteArray
                       -> ST s (MutableByteArray s))
                   -> IO ()
    testByteArray n f = do
        let c :: [Float] = runST $ do
            withByteArray [1..fromIntegral n::Float] $ \aarr -> do
            withByteArray [1..fromIntegral n::Float] $ \barr -> do
            carr <- f n aarr barr
            peekByteArray n carr
        print c
        print $ zipWith  (\a b -> a^2 + b^2)
                         [1..fromIntegral n::Float]
                         [1..fromIntegral n::Float]

withByteArray :: forall a b m . (Prim a, PrimMonad m) => [a] -> (ByteArray -> m b) -> m b
withByteArray xs f = do
    arr <- newByteArray (length xs*sizeOf (undefined :: a))
    loop arr 0 xs
    farr <- unsafeFreezeByteArray arr
    f farr
  where
    loop :: (Prim a, PrimMonad m) => MutableByteArray (PrimState m) -> Int -> [a] -> m ()
    loop _ _ [] = return ()

    loop arr i (x : xs) = do
        writeByteArray arr i x
        loop arr (i+1) xs

peekByteArray :: (Prim a, PrimMonad m)
              => Int
              -> MutableByteArray (PrimState m)
              -> m [a]
peekByteArray n arr =
    loop 0 arr
  where
    loop :: (Prim a, PrimMonad m)
         => Int
         -> MutableByteArray (PrimState m)
         -> m [a]
    loop i _ | i >= n = return []

    loop i arr = do
        x  <- readByteArray arr i
        xs <- loop (i+1) arr
        return (x : xs)

--
-- Roman's original request:
--
-- Let's start with the 3 simple functions that I mentioned. I want to use
-- vector instructions to compute c[i] = a[i]^2 + b[i]^2 in these functions:
--
-- f1 :: ForeignPtr Float -> ForeignPtr Float -> IO (ForeignPtr Float)
-- f2 :: ByteArray -> ByteArray -> ST s (MutableByteArray s)
-- f3 :: [Float] -> [Float] -> [Float]
--
-- In f3, I want to process as many elements at once as necessary.
--
-- Geoff's comment on the solution below:
--
-- I've (naturally) added a parameter to @f1@ and @f2@ that specifies the number
-- of @Float@s in the @ForeignPtr@s/@ByteArray@s. I have also included scalar
-- versions of each function for reference. @Multi Float@s are not useful for
-- manupilating @[Float]@s, so I have not included a definition for @f3@. Note
-- that the code is agnostic with respect to the number of @Float@s in a @Multi
-- Float@.
--

f1_scalar :: Int
          -> ForeignPtr Float
          -> ForeignPtr Float
          -> IO (ForeignPtr Float)
f1_scalar n afptr bfptr =
    withForeignPtr afptr $ \aptr -> do
    withForeignPtr bfptr $ \bptr -> do
    cptr  <- mallocArray n
    loop 0 aptr bptr cptr
    newForeignPtr finalizerFree cptr
  where
    loop :: Int -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()
    loop i _ _ _ | i >= n = return ()

    loop i aptr bptr cptr = do
        a <- peekElemOff aptr i
        b <- peekElemOff bptr i
        pokeElemOff cptr i (a^2 + b^2)
        loop (i+ 1) aptr bptr cptr

f1 :: Int
   -> ForeignPtr Float
   -> ForeignPtr Float
   -> IO (ForeignPtr Float)
f1 n afptr bfptr =
    withForeignPtr afptr $ \aptr -> do
    withForeignPtr bfptr $ \bptr -> do
    cptr  <- mallocArray n
    loop 0 aptr bptr cptr
    newForeignPtr finalizerFree cptr
  where
    k, m :: Int
    m = multiplicity (undefined :: Multi Float)
    k = n - n `rem` m

    loop :: Int -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()
    loop i aptr bptr cptr | i >= k =
        loop1 i aptr bptr cptr

    loop i aptr bptr cptr = do
        a <- peekElemOffAsMulti aptr i
        b <- peekElemOffAsMulti bptr i
        pokeElemOffAsMulti cptr i (a^2 + b^2)
        loop (i + m) aptr bptr cptr

    loop1 :: Int -> Ptr Float -> Ptr Float -> Ptr Float -> IO ()
    loop1 i _ _ _ | i >= n = return ()

    loop1 i aptr bptr cptr = do
        a <- peekElemOff aptr i
        b <- peekElemOff bptr i
        pokeElemOff cptr i (a^2 + b^2)
        loop (i + 1) aptr bptr cptr

f2_scalar :: Int -> ByteArray -> ByteArray -> ST s (MutableByteArray s)
f2_scalar n aarr barr = do
    carr <- newByteArray (n*sizeOf (undefined :: Float))
    loop 0 aarr barr carr
    return carr
  where
    loop :: Int -> ByteArray -> ByteArray -> MutableByteArray s -> ST s ()
    loop i _ _ _ | i >= n =
        return ()

    loop i aarr barr carr = do
        let a = indexByteArray aarr i
            b = indexByteArray barr i
        writeByteArray carr i (a^2 + b^2 :: Float)
        loop (i + 1) aarr barr carr

f2 :: Int -> ByteArray -> ByteArray -> ST s (MutableByteArray s)
f2 n aarr barr = do
    carr <- newByteArray (n*sizeOf (undefined :: Float))
    loop 0 aarr barr carr
    return carr
  where
    k, m :: Int
    m = multiplicity (undefined :: Multi Float)
    k = n - n `rem` m

    loop :: Int -> ByteArray -> ByteArray -> MutableByteArray s -> ST s ()
    loop i aarr barr carr | i >= n =
        loop1 i aarr barr carr

    loop i aarr barr carr = do
        let a = indexByteArrayAsMulti aarr i
            b = indexByteArrayAsMulti barr i
        writeByteArrayAsMulti carr i (a^2 + b^2 :: Multi Float)
        loop (i + 1) aarr barr carr

    loop1 :: Int -> ByteArray -> ByteArray -> MutableByteArray s -> ST s ()
    loop1 i _ _ _ | i >= n =
        return ()

    loop1 i aarr barr carr = do
        let a = indexByteArray aarr i
            b = indexByteArray barr i
        writeByteArray carr i (a^2 + b^2 :: Float)
        loop1 (i + 1) aarr barr carr
