{-# LANGUAGE TupleSections #-}

module Examples.Simple where

import Circuit
import Circuit.Builder
import Circuit.Utils

import Control.Monad

export :: Gate g => [(String, [IO (String, Circuit g)])]
export = [ ("simple",  [("simple",)  <$> return simple])
         , ("and1",    [("and1",)    <$> return andCirc 1])
         , ("and10",   [("and10",)   <$> return andCirc 10])
         , ("and100",  [("and100",)  <$> return andCirc 100])
         , ("and1000", [("and1000",) <$> return andCirc 1000])
         ]

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

