{-# LANGUAGE OverloadedLists #-}

import Circuit
import Circuit.Builder
import qualified Circuit.Format.Acirc as Acirc
import Util

import Control.Monad
import Data.List.Split
import qualified Data.Vector as V

import Debug.Trace


-- note: msb

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

bitEq :: Ref -> Ref -> Ref -> Builder Ref
bitEq one x y = do
    w0 <- circMul x y
    x' <- circSub one x
    y' <- circSub one y
    w1 <- circMul x' y'
    circAdd w0 w1

bitsEq :: Ref -> [Ref] -> [Ref] -> Builder Ref
bitsEq one xs ys
  | length xs /= length ys = error "[bitsEq] unequal length inputs"
  | otherwise = do
    zs <- mapM (uncurry (bitEq one)) (zip xs ys)
    circProd zs

bitSet :: Ref -> Ref -> Bool -> Builder Ref
bitSet one x True  = return x
bitSet one x False = circSub one x

bitsSet :: Ref -> [Ref] -> [Bool] -> Builder Ref
bitsSet one xs bs
  | length xs /= length bs = error "[bitsSet] unequal length inputs"
  | otherwise = do
    zs <- mapM (uncurry (bitSet one)) (zip xs bs)
    circProd zs

toRachael :: Int -> Circuit
toRachael n = buildCircuit $ do
    xs  <- inputs n
    one <- secret 1
    let vals = sequence (replicate n [False, True])
    zs <- mapM (bitsSet one xs) vals
    outputs zs

eq' :: Int -> Circuit
eq' n = buildCircuit $ do
    xs <- inputs n
    ys <- inputs n
    one <- secret 1
    z <- bitsEq one xs ys
    output z

subByte :: Circuit
subByte = buildCircuit $ do
    xs <- inputs 256
    outs <- forM [0..7] $ \j -> do
        let vars = map snd $ filter (\(i,_) -> sbox V.! i V.! j) (zip [0..] xs)
        circSum vars
    outputs outs

subByte' :: IO Circuit
subByte' = do
    rachael <- fst <$> Acirc.readAcirc "toRachael.acirc"
    return $ buildCircuit $ do
        xs <- inputs 8
        rs <- subcircuit' rachael xs
        ys <- subcircuit' subByte rs
        outputs ys

sbox0 :: IO Circuit
sbox0 = do
    sbox <- subByte'
    return $ buildCircuit $ do
        xs <- inputs 8
        ys <- subcircuit' sbox xs
        output (head ys)

sboxsum :: IO Circuit
sboxsum = do
    sbox <- subByte'
    return $ buildCircuit $ do
        one <- secret 1
        xs <- inputs 8
        ys <- subcircuit sbox xs [one]
        z  <- foldM (circXor one) (head ys) (tail ys)
        output z

test :: Circuit
test = buildCircuit $ do
    x <- input
    y <- secret 1
    z <- circXor y x y
    output z

addRoundKey :: Circuit
addRoundKey = buildCircuit $ do
    one <- secret 1
    xs  <- inputs 128
    key <- consts 128
    ys  <- zipWithM (circXor one) xs key
    outputs ys

circXor :: Ref -> Ref -> Ref -> Builder Ref
circXor one x y = do
    two <- circAdd one one
    z  <- circAdd x y
    c  <- circMul x y
    c' <- circMul two c
    circSub z c'

xor :: Circuit
xor = buildCircuit $ do
    one <- secret 1
    (x:y:[]) <- inputs 2
    z <- circXor one x y
    output z

buildAes :: IO Circuit
buildAes = do
    toRachael   <- fst <$> Acirc.readAcirc "toRachael.acirc"
    linearParts <- fst <$> Acirc.readAcirc "linearParts.acirc"
    return $ buildCircuit $ do
        state <- chunksOf 8 <$> inputs 128
        one   <- secret 1
        key   <- secrets (replicate 128 0)

        rs    <- mapM (flip (subcircuit toRachael) [one]) state
        xs    <- concat <$> mapM (subcircuit' subByte) rs
        xs'   <- subcircuit linearParts xs [one]
        xs''  <- subcircuit addRoundKey xs' (one:key)

        output (head xs'')
        {-outputs xs''-}
