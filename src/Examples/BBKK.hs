module Exampls.BBKK where

import Circuit
import Circuit.Builder
import Rand

import Control.Monad

prg :: Int -> Int -> Int -> IO Circuit
prg n m d = do
    when (n <= 4 || mod n 4 /= 0) $ error "n must be a multiple of 4 and greater than 4"
    when (m <= 4 || mod m 4 /= 0) $ error "n must be a multiple of 4 and greater than 4"
    indices <- replicateM (div m 4) $ replicateM d $ randIO (randIntMod (div n 4))
    return $ buildCircuit $ do
        xs <- replicateM (div n 4) $ replicateM 2 (inputs 2)
        forM indices $ \sels -> do
            outs <- foldTreeM matrixMul (map (xs !!) sels)
            outputs (concat outs)
