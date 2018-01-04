module Examples.GGM where

import Examples.Goldreich

import Circuit
import Circuit.Builder
import Circuit.Utils

import Control.Monad
import Control.Monad.Trans
import Data.List.Split

export :: [(String, [(String, IO Acirc)])]
export =
    [ ("big_ggm", [("ggm_4_128.dsl.acirc" , ggm 16 128 16)])

    , ("ggm", [ ("ggm_1_32.dsl.acirc"  , ggm  4  32 16)
              , ("ggm_2_32.dsl.acirc"  , ggm  8  32 16)
              , ("ggm_3_32.dsl.acirc"  , ggm 12  32 16)
              , ("ggm_4_32.dsl.acirc"  , ggm 16  32 16)
              , ("ggm_1_64.dsl.acirc"  , ggm  4  64 16)
              , ("ggm_2_64.dsl.acirc"  , ggm  8  64 16)
              , ("ggm_3_64.dsl.acirc"  , ggm 12  64 16)
              , ("ggm_4_64.dsl.acirc"  , ggm 16  64 16)
              , ("ggm_1_128.dsl.acirc" , ggm  4 128 16)
              , ("ggm_2_128.dsl.acirc" , ggm  8 128 16)
              , ("ggm_3_128.dsl.acirc" , ggm 12 128 16)
              , ("ggm_4_128.dsl.acirc" , ggm 16 128 16)
              ] )

    , ("ggm_sigma", [ ("ggm_sigma_1_16_32.dsl.acirc"  , ggmSigma 1  32 16)
                    , ("ggm_sigma_2_16_32.dsl.acirc"  , ggmSigma 2  32 16)
                    , ("ggm_sigma_3_16_32.dsl.acirc"  , ggmSigma 3  32 16)
                    , ("ggm_sigma_4_16_32.dsl.acirc"  , ggmSigma 4  32 16)
                    , ("ggm_sigma_1_16_64.dsl.acirc"  , ggmSigma 1  64 16)
                    , ("ggm_sigma_2_16_64.dsl.acirc"  , ggmSigma 2  64 16)
                    , ("ggm_sigma_3_16_64.dsl.acirc"  , ggmSigma 3  64 16)
                    , ("ggm_sigma_4_16_64.dsl.acirc"  , ggmSigma 4  64 16)
                    , ("ggm_sigma_1_16_128.dsl.acirc" , ggmSigma 1 128 16)
                    , ("ggm_sigma_2_16_128.dsl.acirc" , ggmSigma 2 128 16)
                    , ("ggm_sigma_3_16_128.dsl.acirc" , ggmSigma 3 128 16)
                    , ("ggm_sigma_4_16_128.dsl.acirc" , ggmSigma 4 128 16)

                    , ("ggm_sigma_1_32_32.dsl.acirc"  , ggmSigma 1  32 32)
                    , ("ggm_sigma_1_32_64.dsl.acirc"  , ggmSigma 1  64 32)
                    , ("ggm_sigma_1_32_128.dsl.acirc" , ggmSigma 1 128 32)
                    , ("ggm_sigma_2_32_32.dsl.acirc"  , ggmSigma 2  32 32)
                    , ("ggm_sigma_2_32_64.dsl.acirc"  , ggmSigma 2  64 32)
                    , ("ggm_sigma_2_32_128.dsl.acirc" , ggmSigma 2 128 32)

                    , ("ggm_sigma_1_64_32.dsl.acirc"  , ggmSigma 1  32 64)
                    , ("ggm_sigma_1_64_64.dsl.acirc"  , ggmSigma 1  64 64)
                    , ("ggm_sigma_1_64_128.dsl.acirc" , ggmSigma 1 128 64)
                    , ("ggm_sigma_2_64_32.dsl.acirc"  , ggmSigma 2  32 64)
                    , ("ggm_sigma_2_64_64.dsl.acirc"  , ggmSigma 2  64 64)
                    , ("ggm_sigma_2_64_128.dsl.acirc" , ggmSigma 2 128 64)
                    ] )

    , ("ggm_sigma_256", [ ("ggm_sigma_1_256_32.dsl.acirc"  , ggmSigma 1  32 256)
                        , ("ggm_sigma_1_256_64.dsl.acirc"  , ggmSigma 1  64 256)
                        , ("ggm_sigma_1_256_128.dsl.acirc" , ggmSigma 1 128 256)
                        , ("ggm_sigma_2_256_32.dsl.acirc"  , ggmSigma 2  32 256)
                        , ("ggm_sigma_2_256_64.dsl.acirc"  , ggmSigma 2  64 256)
                        , ("ggm_sigma_2_256_128.dsl.acirc" , ggmSigma 2 128 256)
                        ] )

    , ("ggm_sigma_1024", [ ("ggm_sigma_2_1024_32.dsl.acirc"  , ggmSigma 2  32 1024)
                         , ("ggm_sigma_2_1024_64.dsl.acirc"  , ggmSigma 2  64 1024)
                         , ("ggm_sigma_2_1024_128.dsl.acirc" , ggmSigma 2 128 1024)
                         ] )
    ]

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
