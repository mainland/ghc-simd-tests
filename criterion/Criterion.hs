{-# LANGUAGE ScopedTypeVariables #-}

import Control.Exception (evaluate)
import Control.Monad.Trans (liftIO)
import Criterion.Config
import Criterion.Main
import System.IO (hFlush, stdout)
import System.Random (newStdGen, randomR)

import Util

import qualified Dotp.Single
import qualified Dotp.Multi
import qualified Dotp.NewMulti
import qualified Dotp.Hand

import qualified Saxpy.Single
import qualified Saxpy.Multi
import qualified Saxpy.NewMulti

main = do
    -- generate random input vectors
    g      <- newStdGen
    let a  =  fst (randomR range g)
    u      <- randomU n range
    v      <- randomU n range

    let config     = defaultConfig
    -- force the evaluation of the input vectors
    let prepare    = liftIO $ do  putStr "Generating random vectors..."
                                  hFlush stdout
                                  evaluate a
                                  evaluate u
                                  evaluate v
                                  putStrLn "done."
                                  hFlush stdout
    let benchmarks = [ bgroup "dotp"
           [ bench "dotp"                         $ whnf (uncurry Dotp.Single.dotp)   (u, v)
           , bench "hand-written SIMD dotp"       $ whnf (uncurry Dotp.Hand.dotp)     (u, v)
           , bench "vector library SIMD dotp"     $ whnf (uncurry Dotp.NewMulti.dotp) (u, v)
           , bench "old vector library SIMD dotp" $ whnf (uncurry Dotp.Multi.dotp)    (u, v)
           ], bgroup "saxpy"
           [ bench "saxpy"                         $ whnf (uncurry (Saxpy.Single.saxpy a))   (u, v)
           , bench "vector library SIMD saxpy"     $ whnf (uncurry (Saxpy.NewMulti.saxpy a)) (u, v)
           , bench "old vector library SIMD saxpy" $ whnf (uncurry (Saxpy.Multi.saxpy a))    (u, v)
           ] ]

    defaultMainWith config prepare benchmarks
  where
    n     = 10000000     -- vector length
    range = (-100, 100)  -- range of vector elements
