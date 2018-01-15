module Examples.Simple where

import Circuit
import Circuit.Builder
import Circuit.Utils

import Control.Monad

export = [("simple", [("simple.acirc2", return simple :: IO Acirc2)])]

andCirc :: Gate g => Int -> Circuit g
andCirc n = buildCircuit (inputs (n+1) >>= foldM1 circMul >>= output)

simple :: Gate g => Circuit g
simple = buildCircuit $ do
    x1  <- symbol 2
    x2  <- symbol 3
    ys  <- secrets [0,1,0,1,0]
    one <- constant 1
    w   <- circProd =<< zipWithM circAdd (x1++x2) ys
    z   <- circAdd w one
    output z

