{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Strict #-}

module Circuits.Goldreich where

import Circuit
import Circuit.Builder
import qualified Circuit.Format.Acirc as Acirc
import Util
import Rand

import Control.Monad
import Data.List.Split
import Debug.Trace

make :: IO ()
make = do
    Acirc.writeAcirc "prg_16_16.dsl.acirc" =<< prg 16 16
    Acirc.writeAcirc "prg_16_32.dsl.acirc" =<< prg 16 32
    Acirc.writeAcirc "prg_16_48.dsl.acirc" =<< prg 16 48
    Acirc.writeAcirc "prg_16_64.dsl.acirc" =<< prg 16 64
    Acirc.writeAcirc "prg_32_32.dsl.acirc" =<< prg 32 32
    Acirc.writeAcirc "prg_32_64.dsl.acirc" =<< prg 32 64
    Acirc.writeAcirc "prg_32_96.dsl.acirc" =<< prg 32 96
    Acirc.writeAcirc "prg_32_128.dsl.acirc" =<< prg 32 128

--------------------------------------------------------------------------------
-- f1

majorityNaive :: [Ref] -> Builder Ref
majorityNaive xs = do
    let cs = combinations (length xs `div` 2) xs
    zs <- mapM circProd cs
    circOrs zs

majority :: [Ref] -> Builder Ref
majority xs = lookupTable maj xs
  where
    maj xs = sum (map b2i xs) >= (length xs `div` 2)

xorMaj :: [Ref] -> Builder Ref
xorMaj xs = do
    let n = length xs `div` 2
    wl <- circXors (take n xs)
    -- wr <- majorityNaive (drop n xs)
    wr <- majority (drop n xs)
    circXor wl wr

-- select the ix'th bit from x
select :: [Ref] -> [Ref] -> Builder Ref
select xs ix = do
    sel <- selectionVector ix
    zs  <- zipWithM (circMul) sel xs
    circSum zs

selects :: [Ref] -> [[Ref]] -> Builder [Ref]
selects xs ixs = mapM (select xs) ixs

f1 :: Int -> Int -> IO Circuit
f1 n m = do
    keyBits <- randKeyIO n
    return $ buildCircuit $ do
        let l = ceiling (logBase 2 (fromIntegral n))
            d = l
        key <- secrets keyBits
        zs  <- replicateM m $ do
            xs <- replicateM d (inputs l)
            bs <- selects key xs
            xorMaj bs
        outputs zs

f1_rachel :: Int -> Int -> IO Circuit
f1_rachel n m = do
    keyBits <- randKeyIO n
    return $ buildCircuit $ do
        let d = ceiling (logBase 2 (fromIntegral n))
        key <- secrets keyBits
        zs  <- replicateM m $ do
            xs <- replicateM d (inputs n)
            bs <- mapM (zipWithM circMul key) xs
            zs <- mapM circSum bs
            xorMaj zs
        outputs zs

maj8n :: Circuit
maj8n = buildCircuit (output =<< majorityNaive =<< inputs 8)

maj8 :: Circuit
maj8 = buildCircuit (output =<< majority =<< inputs 8)

xormaj16 :: Circuit
xormaj16 = buildCircuit (output =<< xorMaj =<< inputs 16)

f1_128 :: IO Circuit
f1_128 = f1 128 1

--------------------------------------------------------------------------------
-- f2

f2 :: Int -> Int -> IO Circuit
f2 n m = do
    keyBits <- randKeyIO n
    let l = ceiling (logBase 2 (fromIntegral n))
        d = l
    ext <- genExt (2*m) m
    return $ buildCircuit $ do
        kf <- secrets keyBits
        zs <- replicateM (2*m) $ do
            xs <- replicateM d (inputs l)
            bs <- selects kf xs
            xorMaj bs
        ws <- subcircuit ext zs
        outputs ws

genExt :: Int -> Int -> IO Circuit
genExt ninputs noutputs = do
    key <- randKeyIO (ninputs * noutputs)
    return $ buildCircuit $ do
        x <- inputs ninputs
        a <- chunksOf ninputs <$> secrets key
        z <- matrixTimesVect a x
        outputs z

--------------------------------------------------------------------------------
-- f3

f3 :: Int -> Int -> IO Circuit
f3 n m = do
    -- n is K_f size
    keyBits <- randKeyIO n
    let l = ceiling (logBase 2 (fromIntegral n))
        ninputs = 2*m*(l^2)
    ext <- genExt (2*m) m -- goes from m output bits to m/2 output bits
    mapper <- loadMapper ninputs
    return $ buildCircuit $ do
        kf <- secrets keyBits
        xs <- subcircuit mapper =<< inputs ninputs
        zs <- forM (chunksOf (l^2) xs) $ \x -> do
            bs <- selects kf (chunksOf l x)
            xorMaj bs
        ws <- subcircuit ext zs
        outputs ws

loadMapper :: Int -> IO Circuit
loadMapper n = do
    (c,_) <- Acirc.readAcirc ("mappers/mapper_" ++ show n ++ ".acirc")
    k1 <- randKeyIO n
    k2 <- randKeyIO n
    return $ buildCircuit $ do
        xs <- inputs n
        ks <- secrets ([1] ++ k1 ++ k2)
        zs <- subcircuit' c xs ks
        outputs zs

genMapper :: Int -> IO Circuit
genMapper n = do
    k1 <- randKeyIO n
    k2 <- randKeyIO n
    let f n bs = polyDiv (take n bs) (zipWith xor (drop n bs) (drop (2*n) bs))
    return $ buildCircuit $ do
        xs <- inputs n
        k1 <- secrets k1
        k2 <- secrets k2
        zs <- lookupTableMultibit (f n) (k1 ++ k2 ++ xs)
        outputs zs

polyDiv :: [Bool] -> [Bool] -> [Bool]
polyDiv x y = undefined

--------------------------------------------------------------------------------
-- prg

selectsPt :: [Int] -> [Ref] -> Builder [Ref]
selectsPt sels xs = return (map (xs!!) sels)

prg :: Int -> Int -> IO Circuit
prg n m = do
    let l = numBits n
        d = l
    selections <- replicateM m $ replicateM d (randIO (randIntegerMod (fromIntegral n)))
    return $ buildCircuit $ do
        xs  <- inputs n
        zs  <- forM selections $ \s -> xorMaj =<< selectsPt (map fromIntegral s) xs
        outputs zs

prgKey :: Int -> Int -> IO Circuit
prgKey n m = do
    let l = numBits n
        d = l
    keyBits <- randKeyIO n
    selections <- replicateM m $ replicateM d (randIO (randIntegerMod (fromIntegral n)))
    return $ buildCircuit $ do
        x   <- input
        xs  <- secrets keyBits
        zs  <- forM selections $ \s -> xorMaj =<< selectsPt (map fromIntegral s) xs
        outputs zs


--------------------------------------------------------------------------------
-- ggm

-- choose the ith set from xs
choose :: [Ref] -> [[Ref]] -> Builder [Ref]
choose ix xs = do
    s  <- selectionVector ix
    ws <- zipWithM (\b x -> mapM (circMul b) x) s xs
    mapM circSum (transpose ws)

ggmStep :: Circuit -> [Ref] -> [Ref] -> Builder [Ref]
ggmStep prg seed choice = do
    let n = length seed
    ws <- chunksOf n <$> subcircuit prg seed
    choose choice ws

ggm :: Int -> Int -> Int -> IO Circuit
ggm inputLength keyLength stretch = do
    g <- prg keyLength (stretch * keyLength)
    keyBits <- randKeyIO keyLength
    return $ buildCircuit $ do
        xs   <- inputs inputLength
        seed <- secrets keyBits
        res  <- foldM (ggmStep g) seed (chunksOf (numBits stretch) xs)
        outputs res

--------------------------------------------------------------------------------
-- ggm rachel

ggmStepR :: Circuit -> [Ref] -> [Ref] -> Builder [Ref]
ggmStepR prg seed choice = do
    let n = length seed
    xs <- chunksOf n <$> subcircuit prg seed
    when (length choice /= length xs) $ error "[ggmStepR] wrong input length"
    ws <- zipWithM (\b x -> mapM (circMul b) x) choice xs
    mapM circSum (transpose ws)

ggmRachel :: Int -> Int -> Int -> IO Circuit
ggmRachel inputLength keyLength stretch = do
    g <- prg keyLength (stretch * keyLength)
    keyBits <- randKeyIO keyLength
    return $ buildCircuit $ do
        xs   <- inputs inputLength
        seed <- secrets keyBits
        when ((length xs `mod` stretch) /= 0) $ error "[ggmRachel] wrong input length"
        res  <- foldM (ggmStepR g) seed (chunksOf stretch xs)
        outputs res
