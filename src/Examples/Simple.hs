{-# LANGUAGE TupleSections #-}

module Examples.Simple where

import Circuit
import Circuit.Builder
import Circuit.Utils

import Control.Monad

export :: Gate g => [(String, [IO (String, Circuit g)])]
export = [("simple", [("simple.acirc2",) <$> return simple])]

andCirc :: Gate g => Int -> Circuit g
andCirc n = buildCircuit (inputs (n+1) >>= foldM1 circMul >>= output)

simple :: Gate g => Circuit g
simple = buildCircuit $ do
    x1  <- symbol 2
    x2  <- sigma 3
    ys  <- secrets [0,1,0,1,0]
    one <- constant 1
    w   <- circProd =<< zipWithM circAdd (x1++x2) ys
    w'  <- circNot w
    z   <- circAdd w' one
    output z

