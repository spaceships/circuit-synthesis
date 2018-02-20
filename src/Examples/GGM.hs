{-# LANGUAGE TupleSections #-}

module Examples.GGM where

import Examples.Goldreich

import Circuit
import Circuit.Builder
import Circuit.Utils

import Control.Monad
import Control.Monad.Trans

export :: Gate g => [(String, [IO (String, Circuit g)])]
export =
    [ ("big_ggm", [("ggm_4_128" ,) <$> ggm 16 128 16])

    , ("ggm", [ ("ggm_1_32"  ,) <$> ggm  4  32 16
              , ("ggm_2_32"  ,) <$> ggm  8  32 16
              , ("ggm_3_32"  ,) <$> ggm 12  32 16
              , ("ggm_4_32"  ,) <$> ggm 16  32 16
              , ("ggm_1_64"  ,) <$> ggm  4  64 16
              , ("ggm_2_64"  ,) <$> ggm  8  64 16
              , ("ggm_3_64"  ,) <$> ggm 12  64 16
              , ("ggm_4_64"  ,) <$> ggm 16  64 16
              , ("ggm_1_128" ,) <$> ggm  4 128 16
              , ("ggm_2_128" ,) <$> ggm  8 128 16
              , ("ggm_3_128" ,) <$> ggm 12 128 16
              , ("ggm_4_128" ,) <$> ggm 16 128 16
              ] )

    , ("ggm_sigma", [ ("ggm_sigma_1_16_32"  ,) <$> ggmSigma 1  32 16
                    , ("ggm_sigma_2_16_32"  ,) <$> ggmSigma 2  32 16
                    , ("ggm_sigma_3_16_32"  ,) <$> ggmSigma 3  32 16
                    , ("ggm_sigma_4_16_32"  ,) <$> ggmSigma 4  32 16
                    , ("ggm_sigma_1_16_64"  ,) <$> ggmSigma 1  64 16
                    , ("ggm_sigma_2_16_64"  ,) <$> ggmSigma 2  64 16
                    , ("ggm_sigma_3_16_64"  ,) <$> ggmSigma 3  64 16
                    , ("ggm_sigma_4_16_64"  ,) <$> ggmSigma 4  64 16
                    , ("ggm_sigma_1_16_128" ,) <$> ggmSigma 1 128 16
                    , ("ggm_sigma_2_16_128" ,) <$> ggmSigma 2 128 16
                    , ("ggm_sigma_3_16_128" ,) <$> ggmSigma 3 128 16
                    , ("ggm_sigma_4_16_128" ,) <$> ggmSigma 4 128 16

                    , ("ggm_sigma_1_32_32"  ,) <$> ggmSigma 1  32 32
                    , ("ggm_sigma_1_32_64"  ,) <$> ggmSigma 1  64 32
                    , ("ggm_sigma_1_32_128" ,) <$> ggmSigma 1 128 32
                    , ("ggm_sigma_2_32_32"  ,) <$> ggmSigma 2  32 32
                    , ("ggm_sigma_2_32_64"  ,) <$> ggmSigma 2  64 32
                    , ("ggm_sigma_2_32_128" ,) <$> ggmSigma 2 128 32

                    , ("ggm_sigma_1_64_32"  ,) <$> ggmSigma 1  32 64
                    , ("ggm_sigma_1_64_64"  ,) <$> ggmSigma 1  64 64
                    , ("ggm_sigma_1_64_128" ,) <$> ggmSigma 1 128 64
                    , ("ggm_sigma_2_64_32"  ,) <$> ggmSigma 2  32 64
                    , ("ggm_sigma_2_64_64"  ,) <$> ggmSigma 2  64 64
                    , ("ggm_sigma_2_64_128" ,) <$> ggmSigma 2 128 64
                    ] )

    , ("ggm_sigma_256", [ ("ggm_sigma_1_256_32"  ,) <$> ggmSigma 1  32 256
                        , ("ggm_sigma_1_256_64"  ,) <$> ggmSigma 1  64 256
                        , ("ggm_sigma_1_256_128" ,) <$> ggmSigma 1 128 256
                        , ("ggm_sigma_2_256_32"  ,) <$> ggmSigma 2  32 256
                        , ("ggm_sigma_2_256_64"  ,) <$> ggmSigma 2  64 256
                        , ("ggm_sigma_2_256_128" ,) <$> ggmSigma 2 128 256
                        ] )

    , ("ggm_sigma_1024", [ ("ggm_sigma_2_1024_32"  ,) <$> ggmSigma 2  32 1024
                         , ("ggm_sigma_2_1024_64"  ,) <$> ggmSigma 2  64 1024
                         , ("ggm_sigma_2_1024_128" ,) <$> ggmSigma 2 128 1024
                         ] )
    ]

--------------------------------------------------------------------------------
-- ggm

-- choose the ith set from xs
choose :: (Gate g, Monad m) => [Ref] -> [[Ref]] -> BuilderT g m [Ref]
choose ix xs = do
    s  <- selectionVector ix
    ws <- zipWithM (\b x -> mapM (circMul b) x) s xs
    mapM circSum (transpose ws)

ggmStep :: (Gate g, Monad m) => Circuit g -> [Ref] -> [Ref] -> BuilderT g m [Ref]
ggmStep prg seed choice = do
    let n = length seed
    ws <- safeChunksOf n <$> subcircuit prg seed
    choose choice ws

ggm :: Gate g => Int -> Int -> Int -> IO (Circuit g)
ggm inputLength keyLength stretch = buildCircuitT $ do
    g <- lift $ prg' keyLength (stretch * keyLength) 5 xorAnd
    keyBits <- lift $ randKeyIO keyLength
    xs   <- inputs inputLength
    seed <- secrets keyBits
    res  <- foldM (ggmStep g) seed (safeChunksOf (numBits stretch) xs)
    outputs res

ggmNoPrg :: Gate g => Int -> Int -> Int -> IO (Circuit g)
ggmNoPrg inputLength keyLength stretch = buildCircuitT $ do
    let g = buildCircuit $ do
                xs <- inputs keyLength
                replicateM stretch (outputs xs)
    keyBits <- lift $ randKeyIO keyLength
    xs   <- inputs inputLength
    seed <- secrets keyBits
    res  <- foldM (ggmStep g) seed (safeChunksOf (numBits stretch) xs)
    outputs res

--------------------------------------------------------------------------------
-- ggm rachel

ggmStepR :: (Gate g, Monad m) => Circuit g -> [Ref] -> [Ref] -> BuilderT g m [Ref]
ggmStepR prg seed choice = do
    let n = length seed
    xs <- safeChunksOf n <$> subcircuit prg seed
    when (length choice /= length xs) $ error "[ggmStepR] wrong input length"
    ws <- zipWithM (\b x -> mapM (circMul b) x) choice xs
    mapM circSum (transpose ws)

-- set noutputs= logBase 2 symlen * num_prg
ggmSigma :: Gate g => Int -> Int -> Int -> IO (Circuit g)
ggmSigma num_prg keyLength symlen = buildCircuitT $ do
    let outputLength = numBits symlen * num_prg
    g <- lift $ prg' keyLength (keyLength * symlen) 5 xorAnd
    keyBits <- lift $ randKeyIO keyLength
    setSigma 0
    xs   <- replicateM num_prg (symbol symlen)
    seed <- secrets keyBits
    res  <- foldM (ggmStepR g) seed xs
    outputs (take outputLength res)
