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
    one <- constant 1
    let vals = sequence (replicate n [False, True])
    zs <- mapM (bitsSet one xs) vals
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
    rs <- subcircuit (toRachael 8) xs
    ys <- subcircuit subByteFromRachael rs
    outputs ys

buildAes :: Int -> IO Circuit
buildAes n = do
    linearParts <- fst <$> Acirc.readAcirc "linearParts.acirc"
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
        {-output (head xs'')-}
        outputs xs''

sbox0 :: Circuit
sbox0 = buildCircuit $ do
    xs <- inputs 8
    ys <- subcircuit subByte xs
    output (head ys)

sboxsum :: Circuit
sboxsum = buildCircuit $ do
    xs <- inputs 8
    ys <- subcircuit subByte xs
    z  <- circXors ys
    output z

xor :: Circuit
xor = buildCircuit $ do
    x <- input
    y <- input
    z <- circXor x y
    output z

