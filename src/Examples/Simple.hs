{-# LANGUAGE TupleSections #-}

module Examples.Simple where

import Circuit
import Circuit.Builder
import Circuit.Utils

import Control.Monad

export :: Gate g => [(String, [IO (String, Circuit g)])]
export = [ ("simple",    [("simple",)    <$> return simple])
         , ("and1",      [("and1",)      <$> return (andCirc 1)])
         , ("and10",     [("and10",)     <$> return (andCirc 10)])
         , ("and100",    [("and100",)    <$> return (andCirc 100)])
         , ("and1000",   [("and1000",)   <$> return (andCirc 1000)])
         , ("xor1",      [("xor1",)      <$> return (xorCirc 1)])
         , ("xor10",     [("xor10",)     <$> return (xorCirc 10)])
         , ("xor100",    [("xor100",)    <$> return (xorCirc 100)])
         , ("xor1000",   [("xor1000",)   <$> return (xorCirc 1000)])
         , ("xorand2",      [("xorand2",)      <$> return (xorAndCirc 2)])
         , ("xorand10",     [("xorand10",)     <$> return (xorAndCirc 10)])
         , ("xorand50",     [("xorand50",)     <$> return (xorAndCirc 50)])
         , ("xorand100",    [("xorand100",)    <$> return (xorAndCirc 100)])
         , ("xorand1000",   [("xorand1000",)   <$> return (xorAndCirc 1000)])
         , ("simple2",   [("simple2",)   <$> return simple2])
         , ("sym1",   [("sym1",)   <$> return (simpleSym 1)])
         , ("sym2",   [("sym2",)   <$> return (simpleSym 2)])
         , ("sym10",  [("sym10",)  <$> return (simpleSym 10)])
         , ("sym100", [("sym100",) <$> return (simpleSym 100)])
         , ("and-secret", [("and-secret",) <$> return andSecret])
         , ("and-const", [("and-const",) <$> return andConst])
         , ("xor-secret", [("xor-secret",) <$> return xorSecret])
         , ("xor-const", [("xor-const",) <$> return xorConst])
         , ("negation", [("negation",) <$> return negation])
         ]

xorCirc :: Gate g => Int -> Circuit g
xorCirc n = buildCircuit (symbol (n+1) >>= foldM1 circXor >>= output)

andCirc :: Gate g => Int -> Circuit g
andCirc n = buildCircuit (symbol (n+1) >>= foldM1 circMul >>= output)

andSecret :: Gate g => Circuit g
andSecret = buildCircuit $ do
    x <- input
    y <- secret 1
    output =<< circAnd x y

andConst :: Gate g => Circuit g
andConst = buildCircuit $ do
    x <- input
    y <- constant 1
    output =<< circMul x y

xorSecret :: Gate g => Circuit g
xorSecret = buildCircuit $ do
    x <- input
    y <- secret 1
    output =<< circXor x y

xorConst :: Gate g => Circuit g
xorConst = buildCircuit $ do
    x <- input
    y <- constant 1
    output =<< circXor x y

xorAndCirc :: Gate g => Int -> Circuit g
xorAndCirc n = buildCircuit $ do
    [x,y] <- safeChunksOf (div n 2) <$> symbol n
    output =<< circProd =<< zipWithM circXor x y

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

simple2 :: Gate g => Circuit g
simple2 = buildCircuit $ do
    [x,y] <- safeChunksOf 4 <$> symbol 8
    d <- zipWithM circXor x y
    output =<< circProd d

simpleSym :: Gate g => Int -> Circuit g
simpleSym n = buildCircuit $ do
    xs <- symbol n
    ys <- symbol n
    zs <- zipWithM circXor xs ys
    output =<< circProd zs

negation :: Gate g => Circuit g
negation = buildCircuit $ output =<< circNot =<< input
