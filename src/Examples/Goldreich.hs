{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE Strict #-}
#endif

module Examples.Goldreich where

import Circuit
import Circuit.Builder
import Circuit.Utils

import Control.Monad
import Control.Monad.Trans
import Data.List.Split

makePRG :: IO [(Maybe String, Acirc)]
makePRG = sequence
    [ (Just "prg_xormaj_32_32.dsl.acirc"   ,) <$> prg'  32  32 5 xorMaj
    , (Just "prg_xormaj_32_128.dsl.acirc"  ,) <$> prg'  32 128 5 xorMaj
    , (Just "prg_xormaj_64_64.dsl.acirc"   ,) <$> prg'  64  64 6 xorMaj
    , (Just "prg_xormaj_64_128.dsl.acirc"  ,) <$> prg'  64 128 6 xorMaj
    , (Just "prg_xormaj_128_128.dsl.acirc" ,) <$> prg' 128 128 7 xorMaj

    , (Just "prg_xorand_32_32.dsl.acirc"   ,) <$> prg'  32  32 5 xorAnd
    , (Just "prg_xorand_32_128.dsl.acirc"  ,) <$> prg'  32 128 5 xorAnd
    , (Just "prg_xorand_64_64.dsl.acirc"   ,) <$> prg'  64  64 5 xorAnd
    , (Just "prg_xorand_64_128.dsl.acirc"  ,) <$> prg'  64 128 5 xorAnd
    , (Just "prg_xorand_128_128.dsl.acirc" ,) <$> prg' 128 128 5 xorAnd
    ]

makeGGM :: IO [(Maybe String, Acirc)]
makeGGM = sequence
    [ (Just "ggm_1_32.dsl.acirc"  ,) <$> ggm  4  32 16
    , (Just "ggm_2_32.dsl.acirc"  ,) <$> ggm  8  32 16
    , (Just "ggm_3_32.dsl.acirc"  ,) <$> ggm 12  32 16
    , (Just "ggm_4_32.dsl.acirc"  ,) <$> ggm 16  32 16
    , (Just "ggm_1_64.dsl.acirc"  ,) <$> ggm  4  64 16
    , (Just "ggm_2_64.dsl.acirc"  ,) <$> ggm  8  64 16
    , (Just "ggm_3_64.dsl.acirc"  ,) <$> ggm 12  64 16
    , (Just "ggm_4_64.dsl.acirc"  ,) <$> ggm 16  64 16
    , (Just "ggm_1_128.dsl.acirc" ,) <$> ggm  4 128 16
    , (Just "ggm_2_128.dsl.acirc" ,) <$> ggm  8 128 16
    , (Just "ggm_3_128.dsl.acirc" ,) <$> ggm 12 128 16
    , (Just "ggm_4_128.dsl.acirc" ,) <$> ggm 16 128 16
    ]

makeGGMSigma :: IO [(Maybe String, Acirc)]
makeGGMSigma = sequence
    [ (Just "ggm_sigma_1_16_32.dsl.acirc"  ,) <$> ggmSigma 1  32 16
    , (Just "ggm_sigma_2_16_32.dsl.acirc"  ,) <$> ggmSigma 2  32 16
    , (Just "ggm_sigma_3_16_32.dsl.acirc"  ,) <$> ggmSigma 3  32 16
    , (Just "ggm_sigma_4_16_32.dsl.acirc"  ,) <$> ggmSigma 4  32 16
    , (Just "ggm_sigma_1_16_64.dsl.acirc"  ,) <$> ggmSigma 1  64 16
    , (Just "ggm_sigma_2_16_64.dsl.acirc"  ,) <$> ggmSigma 2  64 16
    , (Just "ggm_sigma_3_16_64.dsl.acirc"  ,) <$> ggmSigma 3  64 16
    , (Just "ggm_sigma_4_16_64.dsl.acirc"  ,) <$> ggmSigma 4  64 16
    , (Just "ggm_sigma_1_16_128.dsl.acirc" ,) <$> ggmSigma 1 128 16
    , (Just "ggm_sigma_2_16_128.dsl.acirc" ,) <$> ggmSigma 2 128 16
    , (Just "ggm_sigma_3_16_128.dsl.acirc" ,) <$> ggmSigma 3 128 16
    , (Just "ggm_sigma_4_16_128.dsl.acirc" ,) <$> ggmSigma 4 128 16

    , (Just "ggm_sigma_1_32_32.dsl.acirc"  ,) <$> ggmSigma 1  32 32
    , (Just "ggm_sigma_1_32_64.dsl.acirc"  ,) <$> ggmSigma 1  64 32
    , (Just "ggm_sigma_1_32_128.dsl.acirc" ,) <$> ggmSigma 1 128 32
    , (Just "ggm_sigma_2_32_32.dsl.acirc"  ,) <$> ggmSigma 2  32 32
    , (Just "ggm_sigma_2_32_64.dsl.acirc"  ,) <$> ggmSigma 2  64 32
    , (Just "ggm_sigma_2_32_128.dsl.acirc" ,) <$> ggmSigma 2 128 32

    , (Just "ggm_sigma_1_64_32.dsl.acirc"  ,) <$> ggmSigma 1  32 64
    , (Just "ggm_sigma_1_64_64.dsl.acirc"  ,) <$> ggmSigma 1  64 64
    , (Just "ggm_sigma_1_64_128.dsl.acirc" ,) <$> ggmSigma 1 128 64
    , (Just "ggm_sigma_2_64_32.dsl.acirc"  ,) <$> ggmSigma 2  32 64
    , (Just "ggm_sigma_2_64_64.dsl.acirc"  ,) <$> ggmSigma 2  64 64
    , (Just "ggm_sigma_2_64_128.dsl.acirc" ,) <$> ggmSigma 2 128 64
    ]

makeGGMSigma256 :: IO [(Maybe String, Acirc)]
makeGGMSigma256 = sequence
    [ (Just "ggm_sigma_1_256_32.dsl.acirc"  ,) <$> ggmSigma 1  32 256
    , (Just "ggm_sigma_1_256_64.dsl.acirc"  ,) <$> ggmSigma 1  64 256
    , (Just "ggm_sigma_1_256_128.dsl.acirc" ,) <$> ggmSigma 1 128 256
    , (Just "ggm_sigma_2_256_32.dsl.acirc"  ,) <$> ggmSigma 2  32 256
    , (Just "ggm_sigma_2_256_64.dsl.acirc"  ,) <$> ggmSigma 2  64 256
    , (Just "ggm_sigma_2_256_128.dsl.acirc" ,) <$> ggmSigma 2 128 256
    ]

makeGGMSigma1024 :: IO [(Maybe String, Acirc)]
makeGGMSigma1024 = sequence
    [ (Just "ggm_sigma_2_1024_32.dsl.acirc"  ,) <$> ggmSigma 2  32 1024
    , (Just "ggm_sigma_2_1024_64.dsl.acirc"  ,) <$> ggmSigma 2  64 1024
    , (Just "ggm_sigma_2_1024_128.dsl.acirc" ,) <$> ggmSigma 2 128 1024
    ]

makeGGMNoPrg :: IO [(Maybe String, Acirc)]
makeGGMNoPrg = sequence
    [ (Just "ggm_noprg_1_32.dsl.acirc"  ,) <$> ggmNoPrg 4  32 16
    , (Just "ggm_noprg_2_32.dsl.acirc"  ,) <$> ggmNoPrg 8  32 16
    , (Just "ggm_noprg_3_32.dsl.acirc"  ,) <$> ggmNoPrg 12 32 16
    , (Just "ggm_noprg_4_32.dsl.acirc"  ,) <$> ggmNoPrg 16 32 16
    , (Just "ggm_noprg_1_64.dsl.acirc"  ,) <$> ggmNoPrg 4  64 16
    , (Just "ggm_noprg_2_64.dsl.acirc"  ,) <$> ggmNoPrg 8  64 16
    , (Just "ggm_noprg_3_64.dsl.acirc"  ,) <$> ggmNoPrg 12 64 16
    , (Just "ggm_noprg_4_64.dsl.acirc"  ,) <$> ggmNoPrg 16 64 16
    , (Just "ggm_noprg_1_128.dsl.acirc" ,) <$> ggmNoPrg 4  128 16
    , (Just "ggm_noprg_2_128.dsl.acirc" ,) <$> ggmNoPrg 8  128 16
    , (Just "ggm_noprg_3_128.dsl.acirc" ,) <$> ggmNoPrg 12 128 16
    , (Just "ggm_noprg_4_128.dsl.acirc" ,) <$> ggmNoPrg 16 128 16
    ]

makeGGMNoPrgSigma :: IO [(Maybe String, Acirc)]
makeGGMNoPrgSigma = sequence
    [ (Just "ggm_sigma_noprg_1_32.dsl.acirc"  ,) <$> ggmSigmaNoPrg 16 32 16
    , (Just "ggm_sigma_noprg_2_32.dsl.acirc"  ,) <$> ggmSigmaNoPrg 32 32 16
    , (Just "ggm_sigma_noprg_3_32.dsl.acirc"  ,) <$> ggmSigmaNoPrg 48 32 16
    , (Just "ggm_sigma_noprg_4_32.dsl.acirc"  ,) <$> ggmSigmaNoPrg 64 32 16
    , (Just "ggm_sigma_noprg_1_64.dsl.acirc"  ,) <$> ggmSigmaNoPrg 16 64 16
    , (Just "ggm_sigma_noprg_2_64.dsl.acirc"  ,) <$> ggmSigmaNoPrg 32 64 16
    , (Just "ggm_sigma_noprg_3_64.dsl.acirc"  ,) <$> ggmSigmaNoPrg 48 64 16
    , (Just "ggm_sigma_noprg_4_64.dsl.acirc"  ,) <$> ggmSigmaNoPrg 64 64 16
    , (Just "ggm_sigma_noprg_1_128.dsl.acirc" ,) <$> ggmSigmaNoPrg 16 128 16
    , (Just "ggm_sigma_noprg_2_128.dsl.acirc" ,) <$> ggmSigmaNoPrg 32 128 16
    , (Just "ggm_sigma_noprg_3_128.dsl.acirc" ,) <$> ggmSigmaNoPrg 48 128 16
    , (Just "ggm_sigma_noprg_4_128.dsl.acirc" ,) <$> ggmSigmaNoPrg 64 128 16
    ]

--------------------------------------------------------------------------------
-- predicates

majority :: (Gate g, Monad m) => [Ref] -> BuilderT g m Ref
majority xs = lookupTable maj xs
  where
    maj xs = sum (map b2i xs) >= (length xs `div` 2)

xorMaj :: (Gate g, Monad m) => [Ref] -> BuilderT g m Ref
xorMaj xs = do
    let n = length xs `div` 2
    wl <- circXors (take n xs)
    -- wr <- majorityNaive (drop n xs)
    wr <- majority (drop n xs)
    circXor wl wr

xorAnd :: (Gate g, Monad m) => [Ref] -> BuilderT g m Ref
xorAnd (x0:x1:xs) = do
    y <- circMul x0 x1
    circXors (y : xs)
xorAnd _ = error "[xorAnd] need at least three inputs!"

--------------------------------------------------------------------------------
-- prg

prg :: Gate g => Int -> Int -> IO (Circuit g)
prg n m = prg' n m 5 xorAnd

prg' :: Gate g => Int -> Int -> Int -> ([Ref] -> BuilderT g IO Ref) -> IO (Circuit g)
prg' n m d predicate = buildCircuitT $ do
    xs <- inputs n
    zs <- prgBuilder n m d predicate xs
    outputs zs

prgBuilder :: (Gate g, MonadIO m)
           => Int -> Int -> Int -> ([Ref] -> BuilderT g m Ref) -> [Ref]
           -> BuilderT g m [Ref]
prgBuilder ninputs noutputs locality predicate xs = do
    selections <- liftIO $ replicateM noutputs $ replicateM locality (randIO (randIntMod ninputs))
    forM selections $ \s -> do
        sel <- selectsPT s xs
        predicate sel

prgKey :: Int -> Int -> IO Acirc
prgKey n m = buildCircuitT $ do
    let l = numBits n
        d = l
    keyBits <- lift $ randKeyIO n
    selections <- lift $ replicateM m $ replicateM d (randIO (randIntegerMod (fromIntegral n)))
    xs  <- secrets keyBits
    zs  <- forM selections $ \s -> xorMaj =<< selectsPT (map fromIntegral s) xs
    outputs zs

--------------------------------------------------------------------------------
-- ggm

-- choose the ith set from xs
choose :: Monad m => [Ref] -> [[Ref]] -> BuilderT ArithGate m [Ref]
choose ix xs = do
    s  <- selectionVector ix
    ws <- zipWithM (\b x -> mapM (circMul b) x) s xs
    mapM circSum (transpose ws)

ggmStep :: Monad m => Acirc -> [Ref] -> [Ref] -> BuilderT ArithGate m [Ref]
ggmStep prg seed choice = do
    let n = length seed
    ws <- chunksOf n <$> subcircuit prg seed
    choose choice ws

ggm :: Int -> Int -> Int -> IO Acirc
ggm inputLength keyLength stretch = buildCircuitT $ do
    g <- lift $ prg' keyLength (stretch * keyLength) 5 xorAnd
    keyBits <- lift $ randKeyIO keyLength
    xs   <- inputs inputLength
    seed <- secrets keyBits
    res  <- foldM (ggmStep g) seed (chunksOf (numBits stretch) xs)
    outputs res

ggmNoPrg :: Int -> Int -> Int -> IO Acirc
ggmNoPrg inputLength keyLength stretch = buildCircuitT $ do
    let g = buildCircuit $ do
                xs <- inputs keyLength
                replicateM stretch (outputs xs)
    keyBits <- lift $ randKeyIO keyLength
    xs   <- inputs inputLength
    seed <- secrets keyBits
    res  <- foldM (ggmStep g) seed (chunksOf (numBits stretch) xs)
    outputs res

--------------------------------------------------------------------------------
-- ggm rachel

ggmStepR :: Monad m => Acirc -> [Ref] -> [Ref] -> BuilderT ArithGate m [Ref]
ggmStepR prg seed choice = do
    let n = length seed
    xs <- chunksOf n <$> subcircuit prg seed
    when (length choice /= length xs) $ error "[ggmStepR] wrong input length"
    ws <- zipWithM (\b x -> mapM (circMul b) x) choice xs
    mapM circSum (transpose ws)

-- set noutputs= logBase 2 symlen * num_prg
ggmSigma :: Int -> Int -> Int -> IO Acirc
ggmSigma num_prg keyLength symlen = buildCircuitT $ do
    let outputLength = numBits symlen * num_prg
    g <- lift $ prg' keyLength (keyLength * symlen) 5 xorAnd
    keyBits <- lift $ randKeyIO keyLength
    setSymlen symlen
    xs   <- replicateM num_prg (inputs symlen)
    seed <- secrets keyBits
    res  <- foldM (ggmStepR g) seed xs
    outputs (take outputLength res)

ggmSigmaNoPrg :: Int -> Int -> Int -> IO Acirc
ggmSigmaNoPrg inputLength keyLength stretch = buildCircuitT $ do
    let g = buildCircuit $ do
                xs <- inputs keyLength
                replicateM stretch (outputs xs)
    keyBits <- lift $ randKeyIO keyLength
    xs   <- inputs inputLength
    seed <- secrets keyBits
    when ((length xs `mod` stretch) /= 0) $ error "[ggmSigmaNoPrg] wrong input length"
    res  <- foldM (ggmStepR g) seed (chunksOf stretch xs)
    outputs res
