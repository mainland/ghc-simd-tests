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
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS
import qualified Data.Vector.Unboxed as U
import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import System.Random (Random, newStdGen, randomRs)
import Text.Printf

import Util.MmMalloc

randomMM :: (Storable a,Random a) => Int -> (a, a) -> IO (S.Vector a)
randomMM n range = do
    gen  <- newStdGen
    mvec <- MS.unsafeFromForeignPtr0 <$> mmMallocForeignPtr n 16 <*> return n
    zipWithM_ (\i a -> MS.write mvec i a) [0..] (take n (randomRs range gen))
    S.freeze mvec

randomU :: (U.Unbox a,Random a) => Int -> (a, a) -> IO (U.Vector a)
randomU n range = do
    gen <- newStdGen
    return $ U.fromList $ take n (randomRs range gen)
