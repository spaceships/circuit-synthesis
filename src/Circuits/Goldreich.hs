{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE Strict #-}
#endif

module Circuits.Goldreich where

import Circuit
import Circuit.Builder
import Circuit.Optimizer (foldConsts)
import qualified Circuit.Format.Acirc as Acirc
import Util
import Rand

import Control.Monad
import Data.List.Split

makePRG :: IO ()
makePRG = do
    Acirc.writeAcirc "prg_xm_16_16.dsl.acirc"  =<< foldConsts <$> prg' 16 16 4 xorMaj
    Acirc.writeAcirc "prg_xm_16_128.dsl.acirc"  =<< foldConsts <$> prg' 16 64 4 xorMaj
    Acirc.writeAcirc "prg_xm_32_32.dsl.acirc"  =<< foldConsts <$> prg' 32 32 5 xorMaj
    Acirc.writeAcirc "prg_xm_32_128.dsl.acirc" =<< foldConsts <$> prg' 32 128 5 xorMaj
    Acirc.writeAcirc "prg_xm_64_64.dsl.acirc"  =<< foldConsts <$> prg' 64 64 6 xorMaj
    Acirc.writeAcirc "prg_xm_64_128.dsl.acirc" =<< foldConsts <$> prg' 64 128 6 xorMaj
    Acirc.writeAcirc "prg_xm_128_128.dsl.acirc" =<< foldConsts <$> prg' 128 128 7 xorMaj

    Acirc.writeAcirc "prg_ow_16_16.dsl.acirc"  =<< foldConsts <$> prg' 16 16 5 linPredicate
    Acirc.writeAcirc "prg_ow_16_128.dsl.acirc"  =<< foldConsts <$> prg' 16 64 5 linPredicate
    Acirc.writeAcirc "prg_ow_32_32.dsl.acirc"  =<< foldConsts <$> prg' 32 32 5 linPredicate
    Acirc.writeAcirc "prg_ow_32_128.dsl.acirc" =<< foldConsts <$> prg' 32 128 5 linPredicate
    Acirc.writeAcirc "prg_ow_64_64.dsl.acirc"  =<< foldConsts <$> prg' 64 64 5 linPredicate
    Acirc.writeAcirc "prg_ow_64_128.dsl.acirc" =<< foldConsts <$> prg' 64 128 5 linPredicate
    Acirc.writeAcirc "prg_ow_128_128.dsl.acirc" =<< foldConsts <$> prg' 128 128 5 linPredicate

    Acirc.writeAcirc "prg_dumb_16_16.dsl.acirc"  =<< foldConsts <$> prg' 16 16 5 linearPredicate
    Acirc.writeAcirc "prg_dumb_16_128.dsl.acirc"  =<< foldConsts <$> prg' 16 64 5 linearPredicate
    Acirc.writeAcirc "prg_dumb_32_32.dsl.acirc"  =<< foldConsts <$> prg' 32 32 5 linearPredicate
    Acirc.writeAcirc "prg_dumb_32_128.dsl.acirc" =<< foldConsts <$> prg' 32 128 5 linearPredicate
    Acirc.writeAcirc "prg_dumb_64_64.dsl.acirc"  =<< foldConsts <$> prg' 64 64 5 linearPredicate
    Acirc.writeAcirc "prg_dumb_64_128.dsl.acirc" =<< foldConsts <$> prg' 64 128 5 linearPredicate
    Acirc.writeAcirc "prg_dumb_128_128.dsl.acirc" =<< foldConsts <$> prg' 128 128 5 linearPredicate

makeGGM :: IO ()
makeGGM = do
    Acirc.writeAcirc "ggm_1_16.dsl.acirc" =<< foldConsts <$> ggm 4  16 16
    Acirc.writeAcirc "ggm_2_16.dsl.acirc" =<< foldConsts <$> ggm 8  16 16
    Acirc.writeAcirc "ggm_3_16.dsl.acirc" =<< foldConsts <$> ggm 12 16 16
    Acirc.writeAcirc "ggm_4_16.dsl.acirc" =<< foldConsts <$> ggm 16 16 16
    Acirc.writeAcirc "ggm_1_32.dsl.acirc" =<< foldConsts <$> ggm 4  32 16
    Acirc.writeAcirc "ggm_2_32.dsl.acirc" =<< foldConsts <$> ggm 8  32 16
    Acirc.writeAcirc "ggm_3_32.dsl.acirc" =<< foldConsts <$> ggm 12 32 16
    Acirc.writeAcirc "ggm_4_32.dsl.acirc" =<< foldConsts <$> ggm 16 32 16
    Acirc.writeAcirc "ggm_1_64.dsl.acirc" =<< foldConsts <$> ggm 4  64 16
    Acirc.writeAcirc "ggm_2_64.dsl.acirc" =<< foldConsts <$> ggm 8  64 16
    Acirc.writeAcirc "ggm_3_64.dsl.acirc" =<< foldConsts <$> ggm 12 64 16
    Acirc.writeAcirc "ggm_4_64.dsl.acirc" =<< foldConsts <$> ggm 16 64 16
    Acirc.writeAcirc "ggm_1_128.dsl.acirc" =<< foldConsts <$> ggm 4  128 16
    Acirc.writeAcirc "ggm_2_128.dsl.acirc" =<< foldConsts <$> ggm 8  128 16
    Acirc.writeAcirc "ggm_3_128.dsl.acirc" =<< foldConsts <$> ggm 12 128 16
    Acirc.writeAcirc "ggm_4_128.dsl.acirc" =<< foldConsts <$> ggm 16 128 16

    Acirc.writeAcircR "ggm_sigma_1_16.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 16 16 16
    Acirc.writeAcircR "ggm_sigma_2_16.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 32 16 16
    Acirc.writeAcircR "ggm_sigma_3_16.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 48 16 16
    Acirc.writeAcircR "ggm_sigma_4_16.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 64 16 16
    Acirc.writeAcircR "ggm_sigma_1_32.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 16 32 16
    Acirc.writeAcircR "ggm_sigma_2_32.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 32 32 16
    Acirc.writeAcircR "ggm_sigma_3_32.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 48 32 16
    Acirc.writeAcircR "ggm_sigma_4_32.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 64 32 16
    Acirc.writeAcircR "ggm_sigma_1_64.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 16 64 16
    Acirc.writeAcircR "ggm_sigma_2_64.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 32 64 16
    Acirc.writeAcircR "ggm_sigma_3_64.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 48 64 16
    Acirc.writeAcircR "ggm_sigma_4_64.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 64 64 16
    Acirc.writeAcircR "ggm_sigma_1_128.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 16 128 16
    Acirc.writeAcircR "ggm_sigma_2_128.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 32 128 16
    Acirc.writeAcircR "ggm_sigma_3_128.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 48 128 16
    Acirc.writeAcircR "ggm_sigma_4_128.dsl.acirc" 16 =<< foldConsts <$> ggmRachel 64 128 16


makeGGMNoPrg :: IO ()
makeGGMNoPrg = do
    Acirc.writeAcirc "ggm_noprg_1_16.dsl.acirc" =<< foldConsts <$> ggmNoPrg 4  16 16
    Acirc.writeAcirc "ggm_noprg_2_16.dsl.acirc" =<< foldConsts <$> ggmNoPrg 8  16 16
    Acirc.writeAcirc "ggm_noprg_3_16.dsl.acirc" =<< foldConsts <$> ggmNoPrg 12 16 16
    Acirc.writeAcirc "ggm_noprg_4_16.dsl.acirc" =<< foldConsts <$> ggmNoPrg 16 16 16
    Acirc.writeAcirc "ggm_noprg_1_32.dsl.acirc" =<< foldConsts <$> ggmNoPrg 4  32 16
    Acirc.writeAcirc "ggm_noprg_2_32.dsl.acirc" =<< foldConsts <$> ggmNoPrg 8  32 16
    Acirc.writeAcirc "ggm_noprg_3_32.dsl.acirc" =<< foldConsts <$> ggmNoPrg 12 32 16
    Acirc.writeAcirc "ggm_noprg_4_32.dsl.acirc" =<< foldConsts <$> ggmNoPrg 16 32 16
    Acirc.writeAcirc "ggm_noprg_1_64.dsl.acirc" =<< foldConsts <$> ggmNoPrg 4  64 16
    Acirc.writeAcirc "ggm_noprg_2_64.dsl.acirc" =<< foldConsts <$> ggmNoPrg 8  64 16
    Acirc.writeAcirc "ggm_noprg_3_64.dsl.acirc" =<< foldConsts <$> ggmNoPrg 12 64 16
    Acirc.writeAcirc "ggm_noprg_4_64.dsl.acirc" =<< foldConsts <$> ggmNoPrg 16 64 16
    Acirc.writeAcirc "ggm_noprg_1_128.dsl.acirc" =<< foldConsts <$> ggmNoPrg 4  128 16
    Acirc.writeAcirc "ggm_noprg_2_128.dsl.acirc" =<< foldConsts <$> ggmNoPrg 8  128 16
    Acirc.writeAcirc "ggm_noprg_3_128.dsl.acirc" =<< foldConsts <$> ggmNoPrg 12 128 16
    Acirc.writeAcirc "ggm_noprg_4_128.dsl.acirc" =<< foldConsts <$> ggmNoPrg 16 128 16

    Acirc.writeAcircR "ggm_sigma_noprg_1_16.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 16 16 16
    Acirc.writeAcircR "ggm_sigma_noprg_2_16.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 32 16 16
    Acirc.writeAcircR "ggm_sigma_noprg_3_16.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 48 16 16
    Acirc.writeAcircR "ggm_sigma_noprg_4_16.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 64 16 16
    Acirc.writeAcircR "ggm_sigma_noprg_1_32.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 16 32 16
    Acirc.writeAcircR "ggm_sigma_noprg_2_32.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 32 32 16
    Acirc.writeAcircR "ggm_sigma_noprg_3_32.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 48 32 16
    Acirc.writeAcircR "ggm_sigma_noprg_4_32.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 64 32 16
    Acirc.writeAcircR "ggm_sigma_noprg_1_64.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 16 64 16
    Acirc.writeAcircR "ggm_sigma_noprg_2_64.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 32 64 16
    Acirc.writeAcircR "ggm_sigma_noprg_3_64.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 48 64 16
    Acirc.writeAcircR "ggm_sigma_noprg_4_64.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 64 64 16
    Acirc.writeAcircR "ggm_sigma_noprg_1_128.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 16 128 16
    Acirc.writeAcircR "ggm_sigma_noprg_2_128.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 32 128 16
    Acirc.writeAcircR "ggm_sigma_noprg_3_128.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 48 128 16
    Acirc.writeAcircR "ggm_sigma_noprg_4_128.dsl.acirc" 16 =<< foldConsts <$> ggmRachelNoPrg 64 128 16

makeApplebaum :: IO ()
makeApplebaum = do
    Acirc.writeAcirc "f1_16.dsl.acirc"    =<< foldConsts <$> f1 16 1
    Acirc.writeAcirc "f1_32.dsl.acirc"    =<< foldConsts <$> f1 32 1
    Acirc.writeAcirc "f1_64.dsl.acirc"    =<< foldConsts <$> f1 64 1
    Acirc.writeAcirc "f1_128.dsl.acirc"   =<< foldConsts <$> f1 128 1
    Acirc.writeAcirc "f1_128_2.dsl.acirc" =<< foldConsts <$> f1 128 2
    Acirc.writeAcirc "f3_4.dsl.acirc"     =<< foldConsts <$> f3 4 1

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
        let l = ceiling (logBase 2 (fromIntegral n) :: Double)
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
        let d = ceiling (logBase 2 (fromIntegral n) :: Double)
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
    let l = ceiling (logBase 2 (fromIntegral n) :: Double)
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
    let l = ceiling (logBase 2 (fromIntegral n) :: Double)
        ninputs = 2*m*(l^(2 :: Int))
    ext <- genExt (2*m) m -- goes from m output bits to m/2 output bits
    mapper <- loadMapper ninputs
    return $ buildCircuit $ do
        kf <- secrets keyBits
        xs <- subcircuit mapper =<< inputs ninputs
        zs <- forM (chunksOf (l^(2 :: Int)) xs) $ \x -> do
            bs <- selects kf (chunksOf l x)
            xorMaj bs
        ws <- subcircuit ext zs
        outputs ws

loadMapper :: Int -> IO Circuit
loadMapper n = do
    (c,_) <- Acirc.readAcirc ("mappers/mapper_" ++ show n ++ ".c2v.acirc")
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
polyDiv _ _ = undefined

--------------------------------------------------------------------------------
-- prg

selectsPt :: [Int] -> [Ref] -> Builder [Ref]
selectsPt sels xs = return (map (xs!!) sels)

prg :: Int -> Int -> IO Circuit
prg n m = prg' n m (numBits n) xorMaj

prg' :: Int -> Int -> Int -> ([Ref] -> Builder Ref) -> IO Circuit
prg' n m d predicate = do
    selections <- replicateM m $ replicateM d (randIO (randIntMod n))
    return $ buildCircuit $ do
        xs <- inputs n
        zs <- forM selections $ \s -> do
            sel <- selectsPt s xs
            predicate sel
        outputs zs

linPredicate :: [Ref] -> Builder Ref
linPredicate (x0:x1:xs) = do
    y <- circMul x0 x1
    circXors (y : xs)

linearPredicate :: [Ref] -> Builder Ref
linearPredicate = circXors

prgKey :: Int -> Int -> IO Circuit
prgKey n m = do
    let l = numBits n
        d = l
    keyBits <- randKeyIO n
    selections <- replicateM m $ replicateM d (randIO (randIntegerMod (fromIntegral n)))
    return $ buildCircuit $ do
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

ggmNoPrg :: Int -> Int -> Int -> IO Circuit
ggmNoPrg inputLength keyLength stretch = do
    let g = buildCircuit $ do
                xs <- inputs keyLength
                replicateM stretch (outputs xs)
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

ggmRachelNoPrg :: Int -> Int -> Int -> IO Circuit
ggmRachelNoPrg inputLength keyLength stretch = do
    let g = buildCircuit $ do
                xs <- inputs keyLength
                replicateM stretch (outputs xs)
    keyBits <- randKeyIO keyLength
    return $ buildCircuit $ do
        xs   <- inputs inputLength
        seed <- secrets keyBits
        when ((length xs `mod` stretch) /= 0) $ error "[ggmRachel] wrong input length"
        res  <- foldM (ggmStepR g) seed (chunksOf stretch xs)
        outputs res

