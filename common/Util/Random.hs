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
    randomU
  ) where

import Control.Applicative
import Control.Exception (evaluate)
import Control.Monad (replicateM, zipWithM_)
import Control.Monad.Primitive (PrimState)
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
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
    gen <- newStdGen
    mvec <- MU.new n
    fill mvec gen n 0
  where
    fill :: RandomGen g => U.MVector (PrimState IO) a -> g -> Int -> Int -> IO (U.Vector a)
    fill mvec _ n i | i == n = U.freeze mvec
    fill mvec g n i = do
        let (x, g') = randomR range g
        MU.write mvec i x
        fill mvec g' n (i+1)
