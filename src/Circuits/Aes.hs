{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ParallelListComp #-}

module Circuits.Aes where

import Circuit
import Circuit.Builder
import qualified Circuit.Format.Acirc as Acirc
import Util
import Rand

import Control.Monad
import Data.List.Split
import qualified Data.Vector as V

import Debug.Trace

make :: IO ()
make = do
    Acirc.writeAcirc "aes1r.dsl.acirc" =<< buildAes 128
    Acirc.writeAcirc "b0.dsl.acirc"    =<< aes1Bit 128
    Acirc.writeAcirc "b0_64.dsl.acirc" =<< aes1Bit 64
    Acirc.writeAcirc "b0_32.dsl.acirc" =<< aes1Bit 32
    Acirc.writeAcirc "b0_16.dsl.acirc" =<< aes1Bit 16
    Acirc.writeAcirc "b0_8.dsl.acirc"  =<< aes1Bit 8
    Acirc.writeAcirc "b0_7.dsl.acirc"  =<< aes1Bit 7
    Acirc.writeAcirc "b0_6.dsl.acirc"  =<< aes1Bit 6
    Acirc.writeAcirc "b0_5.dsl.acirc"  =<< aes1Bit 5
    Acirc.writeAcirc "b0_4.dsl.acirc"  =<< aes1Bit 4
    Acirc.writeAcirc "b0_3.dsl.acirc"  =<< aes1Bit 3
    Acirc.writeAcirc "b0_2.dsl.acirc"  =<< aes1Bit 2
    Acirc.writeAcirc "sbox.dsl.acirc"  subByte

sbox :: V.Vector (V.Vector Bool)-- {{{
sbox =
    [ [False, True,  True,  False, False, False, True,  True]
    , [False, True,  True,  True,  True,  True,  False, False]
    , [False, True,  True,  True,  False, True,  True,  True]
    , [False, True,  True,  True,  True,  False, True,  True]
    , [True,  True,  True,  True,  False, False, True,  False]
    , [False, True,  True,  False, True,  False, True,  True]
    , [False, True,  True,  False, True,  True,  True,  True]
    , [True,  True,  False, False, False, True,  False, True]
    , [False, False, True,  True,  False, False, False, False]
    , [False, False, False, False, False, False, False, True]
    , [False, True,  True,  False, False, True,  True,  True]
    , [False, False, True,  False, True,  False, True,  True]
    , [True,  True,  True,  True,  True,  True,  True,  False]
    , [True,  True,  False, True,  False, True,  True,  True]
    , [True,  False, True,  False, True,  False, True,  True]
    , [False, True,  True,  True,  False, True,  True,  False]
    , [True,  True,  False, False, True,  False, True,  False]
    , [True,  False, False, False, False, False, True,  False]
    , [True,  True,  False, False, True,  False, False, True]
    , [False, True,  True,  True,  True,  True,  False, True]
    , [True,  True,  True,  True,  True,  False, True,  False]
    , [False, True,  False, True,  True,  False, False, True]
    , [False, True,  False, False, False, True,  True,  True]
    , [True,  True,  True,  True,  False, False, False, False]
    , [True,  False, True,  False, True,  True,  False, True]
    , [True,  True,  False, True,  False, True,  False, False]
    , [True,  False, True,  False, False, False, True,  False]
    , [True,  False, True,  False, True,  True,  True,  True]
    , [True,  False, False, True,  True,  True,  False, False]
    , [True,  False, True,  False, False, True,  False, False]
    , [False, True,  True,  True,  False, False, True,  False]
    , [True,  True,  False, False, False, False, False, False]
    , [True,  False, True,  True,  False, True,  True,  True]
    , [True,  True,  True,  True,  True,  True,  False, True]
    , [True,  False, False, True,  False, False, True,  True]
    , [False, False, True,  False, False, True,  True,  False]
    , [False, False, True,  True,  False, True,  True,  False]
    , [False, False, True,  True,  True,  True,  True,  True]
    , [True,  True,  True,  True,  False, True,  True,  True]
    , [True,  True,  False, False, True,  True,  False, False]
    , [False, False, True,  True,  False, True,  False, False]
    , [True,  False, True,  False, False, True,  False, True]
    , [True,  True,  True,  False, False, True,  False, True]
    , [True,  True,  True,  True,  False, False, False, True]
    , [False, True,  True,  True,  False, False, False, True]
    , [True,  True,  False, True,  True,  False, False, False]
    , [False, False, True,  True,  False, False, False, True]
    , [False, False, False, True,  False, True,  False, True]
    , [False, False, False, False, False, True,  False, False]
    , [True,  True,  False, False, False, True,  True,  True]
    , [False, False, True,  False, False, False, True,  True]
    , [True,  True,  False, False, False, False, True,  True]
    , [False, False, False, True,  True,  False, False, False]
    , [True,  False, False, True,  False, True,  True,  False]
    , [False, False, False, False, False, True,  False, True]
    , [True,  False, False, True,  True,  False, True,  False]
    , [False, False, False, False, False, True,  True,  True]
    , [False, False, False, True,  False, False, True,  False]
    , [True,  False, False, False, False, False, False, False]
    , [True,  True,  True,  False, False, False, True,  False]
    , [True,  True,  True,  False, True,  False, True,  True]
    , [False, False, True,  False, False, True,  True,  True]
    , [True,  False, True,  True,  False, False, True,  False]
    , [False, True,  True,  True,  False, True,  False, True]
    , [False, False, False, False, True,  False, False, True]
    , [True,  False, False, False, False, False, True,  True]
    , [False, False, True,  False, True,  True,  False, False]
    , [False, False, False, True,  True,  False, True,  False]
    , [False, False, False, True,  True,  False, True,  True]
    , [False, True,  True,  False, True,  True,  True,  False]
    , [False, True,  False, True,  True,  False, True,  False]
    , [True,  False, True,  False, False, False, False, False]
    , [False, True,  False, True,  False, False, True,  False]
    , [False, False, True,  True,  True,  False, True,  True]
    , [True,  True,  False, True,  False, True,  True,  False]
    , [True,  False, True,  True,  False, False, True,  True]
    , [False, False, True,  False, True,  False, False, True]
    , [True,  True,  True,  False, False, False, True,  True]
    , [False, False, True,  False, True,  True,  True,  True]
    , [True,  False, False, False, False, True,  False, False]
    , [False, True,  False, True,  False, False, True,  True]
    , [True,  True,  False, True,  False, False, False, True]
    , [False, False, False, False, False, False, False, False]
    , [True,  True,  True,  False, True,  True,  False, True]
    , [False, False, True,  False, False, False, False, False]
    , [True,  True,  True,  True,  True,  True,  False, False]
    , [True,  False, True,  True,  False, False, False, True]
    , [False, True,  False, True,  True,  False, True,  True]
    , [False, True,  True,  False, True,  False, True,  False]
    , [True,  True,  False, False, True,  False, True,  True]
    , [True,  False, True,  True,  True,  True,  True,  False]
    , [False, False, True,  True,  True,  False, False, True]
    , [False, True,  False, False, True,  False, True,  False]
    , [False, True,  False, False, True,  True,  False, False]
    , [False, True,  False, True,  True,  False, False, False]
    , [True,  True,  False, False, True,  True,  True,  True]
    , [True,  True,  False, True,  False, False, False, False]
    , [True,  True,  True,  False, True,  True,  True,  True]
    , [True,  False, True,  False, True,  False, True,  False]
    , [True,  True,  True,  True,  True,  False, True,  True]
    , [False, True,  False, False, False, False, True,  True]
    , [False, True,  False, False, True,  True,  False, True]
    , [False, False, True,  True,  False, False, True,  True]
    , [True,  False, False, False, False, True,  False, True]
    , [False, True,  False, False, False, True,  False, True]
    , [True,  True,  True,  True,  True,  False, False, True]
    , [False, False, False, False, False, False, True,  False]
    , [False, True,  True,  True,  True,  True,  True,  True]
    , [False, True,  False, True,  False, False, False, False]
    , [False, False, True,  True,  True,  True,  False, False]
    , [True,  False, False, True,  True,  True,  True,  True]
    , [True,  False, True,  False, True,  False, False, False]
    , [False, True,  False, True,  False, False, False, True]
    , [True,  False, True,  False, False, False, True,  True]
    , [False, True,  False, False, False, False, False, False]
    , [True,  False, False, False, True,  True,  True,  True]
    , [True,  False, False, True,  False, False, True,  False]
    , [True,  False, False, True,  True,  True,  False, True]
    , [False, False, True,  True,  True,  False, False, False]
    , [True,  True,  True,  True,  False, True,  False, True]
    , [True,  False, True,  True,  True,  True,  False, False]
    , [True,  False, True,  True,  False, True,  True,  False]
    , [True,  True,  False, True,  True,  False, True,  False]
    , [False, False, True,  False, False, False, False, True]
    , [False, False, False, True,  False, False, False, False]
    , [True,  True,  True,  True,  True,  True,  True,  True]
    , [True,  True,  True,  True,  False, False, True,  True]
    , [True,  True,  False, True,  False, False, True,  False]
    , [True,  True,  False, False, True,  True,  False, True]
    , [False, False, False, False, True,  True,  False, False]
    , [False, False, False, True,  False, False, True,  True]
    , [True,  True,  True,  False, True,  True,  False, False]
    , [False, True,  False, True,  True,  True,  True,  True]
    , [True,  False, False, True,  False, True,  True,  True]
    , [False, True,  False, False, False, True,  False, False]
    , [False, False, False, True,  False, True,  True,  True]
    , [True,  True,  False, False, False, True,  False, False]
    , [True,  False, True,  False, False, True,  True,  True]
    , [False, True,  True,  True,  True,  True,  True,  False]
    , [False, False, True,  True,  True,  True,  False, True]
    , [False, True,  True,  False, False, True,  False, False]
    , [False, True,  False, True,  True,  True,  False, True]
    , [False, False, False, True,  True,  False, False, True]
    , [False, True,  True,  True,  False, False, True,  True]
    , [False, True,  True,  False, False, False, False, False]
    , [True,  False, False, False, False, False, False, True]
    , [False, True,  False, False, True,  True,  True,  True]
    , [True,  True,  False, True,  True,  True,  False, False]
    , [False, False, True,  False, False, False, True,  False]
    , [False, False, True,  False, True,  False, True,  False]
    , [True,  False, False, True,  False, False, False, False]
    , [True,  False, False, False, True,  False, False, False]
    , [False, True,  False, False, False, True,  True,  False]
    , [True,  True,  True,  False, True,  True,  True,  False]
    , [True,  False, True,  True,  True,  False, False, False]
    , [False, False, False, True,  False, True,  False, False]
    , [True,  True,  False, True,  True,  True,  True,  False]
    , [False, True,  False, True,  True,  True,  True,  False]
    , [False, False, False, False, True,  False, True,  True]
    , [True,  True,  False, True,  True,  False, True,  True]
    , [True,  True,  True,  False, False, False, False, False]
    , [False, False, True,  True,  False, False, True,  False]
    , [False, False, True,  True,  True,  False, True,  False]
    , [False, False, False, False, True,  False, True,  False]
    , [False, True,  False, False, True,  False, False, True]
    , [False, False, False, False, False, True,  True,  False]
    , [False, False, True,  False, False, True,  False, False]
    , [False, True,  False, True,  True,  True,  False, False]
    , [True,  True,  False, False, False, False, True,  False]
    , [True,  True,  False, True,  False, False, True,  True]
    , [True,  False, True,  False, True,  True,  False, False]
    , [False, True,  True,  False, False, False, True,  False]
    , [True,  False, False, True,  False, False, False, True]
    , [True,  False, False, True,  False, True,  False, True]
    , [True,  True,  True,  False, False, True,  False, False]
    , [False, True,  True,  True,  True,  False, False, True]
    , [True,  True,  True,  False, False, True,  True,  True]
    , [True,  True,  False, False, True,  False, False, False]
    , [False, False, True,  True,  False, True,  True,  True]
    , [False, True,  True,  False, True,  True,  False, True]
    , [True,  False, False, False, True,  True,  False, True]
    , [True,  True,  False, True,  False, True,  False, True]
    , [False, True,  False, False, True,  True,  True,  False]
    , [True,  False, True,  False, True,  False, False, True]
    , [False, True,  True,  False, True,  True,  False, False]
    , [False, True,  False, True,  False, True,  True,  False]
    , [True,  True,  True,  True,  False, True,  False, False]
    , [True,  True,  True,  False, True,  False, True,  False]
    , [False, True,  True,  False, False, True,  False, True]
    , [False, True,  True,  True,  True,  False, True,  False]
    , [True,  False, True,  False, True,  True,  True,  False]
    , [False, False, False, False, True,  False, False, False]
    , [True,  False, True,  True,  True,  False, True,  False]
    , [False, True,  True,  True,  True,  False, False, False]
    , [False, False, True,  False, False, True,  False, True]
    , [False, False, True,  False, True,  True,  True,  False]
    , [False, False, False, True,  True,  True,  False, False]
    , [True,  False, True,  False, False, True,  True,  False]
    , [True,  False, True,  True,  False, True,  False, False]
    , [True,  True,  False, False, False, True,  True,  False]
    , [True,  True,  True,  False, True,  False, False, False]
    , [True,  True,  False, True,  True,  True,  False, True]
    , [False, True,  True,  True,  False, True,  False, False]
    , [False, False, False, True,  True,  True,  True,  True]
    , [False, True,  False, False, True,  False, True,  True]
    , [True,  False, True,  True,  True,  True,  False, True]
    , [True,  False, False, False, True,  False, True,  True]
    , [True,  False, False, False, True,  False, True,  False]
    , [False, True,  True,  True,  False, False, False, False]
    , [False, False, True,  True,  True,  True,  True,  False]
    , [True,  False, True,  True,  False, True,  False, True]
    , [False, True,  True,  False, False, True,  True,  False]
    , [False, True,  False, False, True,  False, False, False]
    , [False, False, False, False, False, False, True,  True]
    , [True,  True,  True,  True,  False, True,  True,  False]
    , [False, False, False, False, True,  True,  True,  False]
    , [False, True,  True,  False, False, False, False, True]
    , [False, False, True,  True,  False, True,  False, True]
    , [False, True,  False, True,  False, True,  True,  True]
    , [True,  False, True,  True,  True,  False, False, True]
    , [True,  False, False, False, False, True,  True,  False]
    , [True,  True,  False, False, False, False, False, True]
    , [False, False, False, True,  True,  True,  False, True]
    , [True,  False, False, True,  True,  True,  True,  False]
    , [True,  True,  True,  False, False, False, False, True]
    , [True,  True,  True,  True,  True,  False, False, False]
    , [True,  False, False, True,  True,  False, False, False]
    , [False, False, False, True,  False, False, False, True]
    , [False, True,  True,  False, True,  False, False, True]
    , [True,  True,  False, True,  True,  False, False, True]
    , [True,  False, False, False, True,  True,  True,  False]
    , [True,  False, False, True,  False, True,  False, False]
    , [True,  False, False, True,  True,  False, True,  True]
    , [False, False, False, True,  True,  True,  True,  False]
    , [True,  False, False, False, False, True,  True,  True]
    , [True,  True,  True,  False, True,  False, False, True]
    , [True,  True,  False, False, True,  True,  True,  False]
    , [False, True,  False, True,  False, True,  False, True]
    , [False, False, True,  False, True,  False, False, False]
    , [True,  True,  False, True,  True,  True,  True,  True]
    , [True,  False, False, False, True,  True,  False, False]
    , [True,  False, True,  False, False, False, False, True]
    , [True,  False, False, False, True,  False, False, True]
    , [False, False, False, False, True,  True,  False, True]
    , [True,  False, True,  True,  True,  True,  True,  True]
    , [True,  True,  True,  False, False, True,  True,  False]
    , [False, True,  False, False, False, False, True,  False]
    , [False, True,  True,  False, True,  False, False, False]
    , [False, True,  False, False, False, False, False, True]
    , [True,  False, False, True,  True,  False, False, True]
    , [False, False, True,  False, True,  True,  False, True]
    , [False, False, False, False, True,  True,  True,  True]
    , [True,  False, True,  True,  False, False, False, False]
    , [False, True,  False, True,  False, True,  False, False]
    , [True,  False, True,  True,  True,  False, True,  True]
    , [False, False, False, True,  False, True,  True,  False]
    ]
-- }}}

toRachel :: Int -> Circuit
toRachel n = buildCircuit $ do
    xs  <- inputs n
    one <- constant 1
    let vals = sequence (replicate n [False, True])
    zs <- mapM (bitsSet xs) vals
    outputs zs

subByteFromRachael :: Circuit
subByteFromRachael = buildCircuit $ do
    xs <- inputs 256
    outs <- forM [0..7] $ \j -> do
        let vars = map snd $ filter (\(i,_) -> sbox V.! i V.! j) (zip [0..] xs)
        circSum vars
    outputs outs

subByte :: Circuit
subByte = buildCircuit $ do
    xs <- inputs 8
    rs <- subcircuit (toRachel 8) xs
    ys <- subcircuit subByteFromRachael rs
    outputs ys

buildAes :: Int -> IO Circuit
buildAes n = do
    linearParts <- fst <$> Acirc.readAcirc "linearParts.c2a.acirc"
    return $ buildCircuit $ do
        inp  <- inputs n
        one  <- constant 1
        zero <- constant 0
        key  <- secrets (replicate 128 0)
        let fixed = replicate (128 - n) zero
        let state = chunksOf 8 (inp ++ fixed)
        xs   <- concat <$> mapM (subcircuit subByte) state
        xs'  <- subcircuit' linearParts xs [one]
        xs'' <- zipWithM circXor xs' key -- addRoundKey
        outputs xs''

buildAes' :: Int -> IO Circuit
buildAes' n = do
    mixCols <- fst <$> Acirc.readAcirc "mixColumns.c2a.acirc"
    return $ buildCircuit $ do
        inp  <- inputs n
        one  <- constant 1
        zero <- constant 0
        key  <- secrets (replicate 128 0)
        let fixed = replicate (128 - n) zero
        xs   <- concat <$> mapM (subcircuit subByte) (chunksOf 8 (inp ++ fixed))
        xs'  <- subcircuit' mixCols (shiftRows xs) [one]
        xs'' <- zipWithM circXor xs' key -- addRoundKey
        outputs xs''

aes1Bit :: Int -> IO Circuit
aes1Bit n = do
    aes <- buildAes n
    return $ buildCircuit $ do
        inp <- inputs n
        zs <- subcircuit aes inp
        output (head zs)

sbox0 :: Circuit
sbox0 = buildCircuit $ do
    xs <- inputs 8
    ys <- subcircuit subByte xs
    output (head ys)

sboxsum :: IO Circuit
sboxsum = do
    ks <- randKeyIO 8
    return $ buildCircuit $ do
        xs <- inputs 8
        ys <- subcircuit subByte xs
        zs <- zipWithM circXor ys =<< secrets ks
        output =<< circXors zs

xor :: Circuit
xor = buildCircuit $ do
    x <- input
    y <- input
    z <- circXor x y
    output z

shiftRows :: [Ref] -> [Ref]
shiftRows xs = fromState [ rotate n row | row <- toState xs | n <- [0..3] ]
  where
    toState   = transpose . chunksOf 4 . chunksOf 8
    fromState = concat . concat . transpose

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

-- watch out: takes hours to compile
compileGF28Mult :: IO Circuit
compileGF28Mult = do
    gf <- fst <$> Acirc.readAcirc "gf.c2a.acirc"
    return $ buildCircuit $ do
        xs <- inputs 16
        ys <- lookupTableMultibit (plainEval gf) xs
        outputs ys

makeGF28Mult :: IO ()
makeGF28Mult = do
    c <- compileGF28Mult
    Acirc.writeAcirc "gf28-mult-lookup.dsl.acirc" c

-- grab compiled version of the circuit
gf28Mult :: IO Circuit
gf28Mult = fst <$> Acirc.readAcirc "gf28-mult-lookup.dsl.acirc"

gf28DotProduct :: Circuit -> [[Ref]] -> [[Ref]] -> Builder [Ref]
gf28DotProduct multCirc xs ys = do
    let mult (x,y) = subcircuit multCirc (x ++ y)
    ws <- mapM mult (zip xs ys)
    mapM circXors ws

gf28DotProductCirc :: IO Circuit
gf28DotProductCirc = do
    mult <- gf28Mult
    return $ buildCircuit $ do
        xs <- replicateM 4 (inputs 8)
        ys <- replicateM 4 (inputs 8)
        zs <- gf28DotProduct mult xs ys
        outputs zs
