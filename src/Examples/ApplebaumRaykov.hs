{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE Strict #-}
#endif

module Examples.ApplebaumRaykov where

import Circuit
import Circuit.Builder
import Circuit.Utils
import qualified Circuit.Format.Acirc as Acirc
import Examples.Goldreich

import Control.Monad
import Control.Monad.Trans
import Data.List.Split

makeApplebaum =
    [ ("f1_16.dsl.acirc"    ,) <$> f1 16 1
    , ("f1_32.dsl.acirc"    ,) <$> f1 32 1
    , ("f1_64.dsl.acirc"    ,) <$> f1 64 1
    , ("f1_128_1.dsl.acirc" ,) <$> f1 128 1
    , ("f1_128_2.dsl.acirc" ,) <$> f1 128 2
    -- , ("f3_4.dsl.acirc"     ,) <$> f3 4 1
    ]

--------------------------------------------------------------------------------
-- f1

perfectSquare :: Int -> Bool
perfectSquare x = whole (sqrt (fromIntegral x :: Float))
  where
    whole :: Float -> Bool
    whole x = x - (fromIntegral (floor x :: Int) :: Float) == 0.0

f1 :: Int -> Int -> IO Acirc
f1 ninputs noutputs
    | not (perfectSquare ninputs) = error "ninputs should be a perfect square"
    | otherwise = buildCircuitT $ do
        let l = ceiling (sqrt (fromIntegral ninputs / fromIntegral noutputs :: Float))
        keyBits <- lift $ randKeyIO (2^l)
        key <- secrets keyBits
        zs  <- replicateM noutputs $ do
            xs <- replicateM l (inputs l)
            bs <- selects key xs
            xorMaj bs
        outputs zs

f1_rachel :: Int -> Int -> IO Acirc
f1_rachel n m = buildCircuitT $ do
    keyBits <- lift $ randKeyIO n
    let d = ceiling (logBase 2 (fromIntegral n) :: Double)
    key <- secrets keyBits
    zs  <- replicateM m $ do
        xs <- replicateM d (inputs n)
        bs <- mapM (zipWithM circMul key) xs
        zs <- mapM circSum bs
        xorMaj zs
    outputs zs

maj8 :: Acirc
maj8 = buildCircuit (output =<< majority =<< inputs 8)

xormaj16 :: Acirc
xormaj16 = buildCircuit (output =<< xorMaj =<< inputs 16)

f1_128 :: IO Acirc
f1_128 = f1 128 1

--------------------------------------------------------------------------------
-- f2

f2 :: Int -> Int -> IO Acirc
f2 n m = buildCircuitT $ do
    keyBits <- lift $ randKeyIO n
    let l = ceiling (logBase 2 (fromIntegral n) :: Double)
        d = l
    ext <- lift $ genExt (2*m) m
    kf <- secrets keyBits
    zs <- replicateM (2*m) $ do
        xs <- replicateM d (inputs l)
        bs <- selects kf xs
        xorMaj bs
    ws <- subcircuit ext zs
    outputs ws

genExt :: Int -> Int -> IO Acirc
genExt ninputs noutputs = buildCircuitT $ do
    key <- lift $ randKeyIO (ninputs * noutputs)
    x <- inputs ninputs
    a <- chunksOf ninputs <$> secrets key
    z <- matrixTimesVect a x
    outputs z

--------------------------------------------------------------------------------
-- f3

f3 :: Int -> Int -> IO Acirc
f3 n m = buildCircuitT $ do
    -- n is K_f size
    keyBits <- lift $ randKeyIO n
    let l = ceiling (logBase 2 (fromIntegral n) :: Double)
        ninputs = 2*m*(l^(2 :: Int))
    ext <- lift $ genExt (2*m) m -- goes from m output bits to m/2 output bits
    mapper <- lift $ loadMapper ninputs
    kf <- secrets keyBits
    xs <- subcircuit mapper =<< inputs ninputs
    zs <- forM (chunksOf (l^(2 :: Int)) xs) $ \x -> do
        bs <- selects kf (chunksOf l x)
        xorMaj bs
    ws <- subcircuit ext zs
    outputs ws

loadMapper :: Int -> IO Acirc
loadMapper n = buildCircuitT $ do
    c  <- lift $ Acirc.read ("mappers/mapper_" ++ show n ++ ".c2v.acirc")
    k1 <- lift $ randKeyIO n
    k2 <- lift $ randKeyIO n
    xs <- inputs n
    ks <- secrets ([1] ++ k1 ++ k2)
    zs <- subcircuit' c xs ks
    outputs zs

genMapper :: Int -> IO Acirc
genMapper n = buildCircuitT $ do
    k1 <- lift $ randKeyIO n
    k2 <- lift $ randKeyIO n
    let f n bs = polyDiv (take n bs) (zipWith xor (drop n bs) (drop (2*n) bs))
    xs <- inputs n
    k1 <- secrets k1
    k2 <- secrets k2
    zs <- lookupTableMultibit (f n) (k1 ++ k2 ++ xs)
    outputs zs

polyDiv :: [Bool] -> [Bool] -> [Bool]
polyDiv _ _ = undefined
