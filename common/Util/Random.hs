-- |
-- Module      : Util.Random
-- Copyright   : (c) Geoffrey Mainland 2011-2012
-- License     : BSD-style
--
-- Maintainer  : Geoffrey Mainland <mainland@apeiron.net>
-- Stability   : experimental
-- Portability : non-portable

module Util.Random (
    randomU
  ) where

import Control.Exception (evaluate)
import Control.Monad (replicateM)
import Data.Primitive.Addr
import Data.Primitive.ByteArray
import Data.Primitive (sizeOf)
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Data.Vector.Primitive as P
import qualified Data.Vector.Unboxed as U
import Foreign.C
import Foreign.Ptr
import GHC.Ptr
--import System.CPUTime (getCPUTime)
import System.Random (Random, newStdGen, randomRs)
import Text.Printf

randomU :: (U.Unbox a,Random a) => Int -> (a, a) -> IO (U.Vector a)
randomU n range = do
    gen <- newStdGen
    return $ U.fromList $ take n (randomRs range gen)
