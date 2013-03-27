{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Util.Random
-- Copyright   : (c) Geoffrey Mainland 2011-2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Util.Random (
    randomMM,
    randomU,
    randomSparseMatrix
  ) where

import Control.Applicative
import Control.Exception (evaluate)
import Control.Monad (replicateM, zipWithM_)
import Control.Monad.Primitive (PrimState)
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as MU
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import System.Random (Random, RandomGen, newStdGen, randomR)
import Text.Printf

import Util.MmMalloc

randomMM :: forall a . (Storable a,Random a) => Int -> (a, a) -> IO (S.Vector a)
randomMM n range = do
    gen  <- newStdGen
    mvec <- MS.unsafeFromForeignPtr0 <$> mmMallocForeignPtr n 32 <*> return n
    fill mvec gen n 0
  where
    fill :: RandomGen g => S.MVector (PrimState IO) a -> g -> Int -> Int -> IO (S.Vector a)
    fill mvec _ n i | i == n = S.freeze mvec
    fill mvec g n i = do
        let (x, g') = randomR range g
        MS.write mvec i x
        fill mvec g' n (i+1)

randomU :: forall a . (U.Unbox a,Random a) => Int -> (a, a) -> IO (U.Vector a)
randomU n range = do
    gen    <- newStdGen
    (_, v) <- randomU' gen n range
    return v

randomU' :: forall a g . (RandomGen g,U.Unbox a,Random a)
         => g -> Int -> (a, a) -> IO (g, U.Vector a)
randomU' g n range = do
    mvec <- MU.new n
    fill mvec g n 0
  where
    fill :: U.MVector (PrimState IO) a -> g -> Int -> Int -> IO (g, U.Vector a)
    fill mvec g n i | i == n = do
        v <- U.freeze mvec
        return (g, v)

    fill mvec g n i = do
        let (x, g') = randomR range g
        MU.write mvec i x
        fill mvec g' n (i+1)

randomSparseMatrix :: forall a . (U.Unbox a,Random a)
                   => Int -> Int -> (a, a) -> IO (V.Vector (U.Vector (Int, a)))
randomSparseMatrix k n range = do
    g <- newStdGen
    randomRows g n
  where
    randomRows :: RandomGen g => g -> Int -> IO (V.Vector (U.Vector (Int, a)))
    randomRows _ 0 = return V.empty
    randomRows g n = do
        let (g', is :: [Int]) =  randomUniqList g k []
        (g'', v)     <- randomU' g' k range
        rows         <- randomRows g'' (n-1)
        return $ U.zip (U.fromList is) v `V.cons` rows

    randomUniqList :: RandomGen g => g -> Int -> [Int] -> (g, [Int])
    randomUniqList g 0 xs = (g, xs)
    randomUniqList g k xs =
        if x `elem` xs
            then randomUniqList g' k     xs
            else randomUniqList g' (k-1) (x:xs)
      where
        (x, g') = randomR (0,n) g
