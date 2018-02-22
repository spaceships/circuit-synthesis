{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TupleSections #-}

module Examples.AES where

import Circuit
import Circuit.Builder
import Circuit.Utils
import qualified Circuit.Format.Acirc as Acirc
import qualified Circuit.Format.Acirc2 as Acirc2

import Control.Monad
import Control.Monad.Trans (lift)
import qualified Data.Vector as V

exportAcirc :: [(String, [IO (String, Acirc)])]
exportAcirc =   [ ("aes", [ ("aes1r"      ,) <$> buildAesRound 128
                        , ("aes1r_128_1",) <$> aes1Bit 128
                        , ("aes1r_64_1" ,) <$> aes1Bit 64
                        , ("aes1r_32_1" ,) <$> aes1Bit 32
                        , ("aes1r_16_1" ,) <$> aes1Bit 16
                        , ("aes1r_8_1"  ,) <$> aes1Bit 8
                        , ("aes1r_4_1"  ,) <$> aes1Bit 4
                        , ("aes1r_2_1"  ,) <$> aes1Bit 2
                        , ("sbox",) <$> return subByteCirc
                        ] )
                , ("aes1r",  [ ("aes1r",) <$> buildAesRound 128 ] )
                , ("aes10r", [ ("aes",)   <$> buildAes 10 128 ] )
                ]

exportAcirc2 :: [(String, [IO (String, Acirc2)])]
exportAcirc2 =  [ ("aes1r", [ ("aes1r",) <$> buildAesRoundA2 128 ])]

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

subByteFromSelection :: (Gate g, Monad m) => [Ref] -> BuilderT g m [Ref]
subByteFromSelection xs = do
    when (length xs /= 256) $
        error "[subByteFromSelection] need a sigma vector of length 256!"
    forM [0..7] $ \j -> do
        let vars = map snd $ filter (\(i,_) -> sbox V.! i V.! j) (zip [0..] xs)
        circSum vars

subByte :: (Gate g, Monad m) => [Ref] -> BuilderT g m [Ref]
subByte xs = do
    when (length xs /= 8) $
        error "[subByte] expected a length 8 binary input!"
    subByteFromSelection =<< selectionVector xs

subByteCirc :: Circuit ArithGate
subByteCirc = buildCircuit $ outputs =<< subByte =<< inputs 8

buildAesRound :: Int -> IO Acirc
buildAesRound n = buildCircuitT $ do
        linearParts <- lift buildLinearParts
        inp  <- inputs n
        one  <- constant 1
        zero <- constant 0
        key  <- secrets (replicate 128 0)
        let fixed = replicate (128 - n) zero
        let state = safeChunksOf 8 (inp ++ fixed)
        xs   <- concat <$> mapM subByte state
        xs'  <- subcircuit linearParts xs
        xs'' <- zipWithM circXor xs' key -- addRoundKey
        outputs xs''

buildAesRoundA2 :: Int -> IO Acirc2
buildAesRoundA2 n = buildCircuitT $ do
        linearParts <- lift buildLinearPartsA2
        inp  <- inputs n
        one  <- constant 1
        zero <- constant 0
        key  <- secrets (replicate 128 0)
        let fixed = replicate (128 - n) zero
        let state = safeChunksOf 8 (inp ++ fixed)
        xs   <- concat <$> mapM subByte state
        xs'  <- subcircuit linearParts xs
        xs'' <- zipWithM circXor xs' key -- addRoundKey
        outputs xs''

buildAes :: Int -> Int -> IO (Circuit ArithGate)
buildAes nrounds ninputs = buildCircuitT $ do
    aesRound <- lift $ Acirc.read "optimized_circuits/aes1r.o2.acirc"
    inp      <- inputs ninputs
    fixed    <- replicateM (128 - ninputs) (constant 0)
    k0       <- replicateM 128 (secret 0)
    -- round 1
    xs <- zipWithM circXor (inp ++ fixed) k0
    if nrounds > 1 then do
        -- round 2+
        zs <- foldM (\x _ -> subcircuit aesRound x) xs ([2..nrounds] :: [Int])
        outputs zs
    else do
        outputs xs

aes1Bit :: Int -> IO (Circuit ArithGate)
aes1Bit n = buildCircuitT $ do
        aes <- lift $ buildAesRound n
        inp <- inputs n
        zs <- subcircuit aes inp
        output (head zs)

sbox0 :: Circuit ArithGate
sbox0 = buildCircuit $ do
    xs <- inputs 8
    ys <- subByte xs
    output (head ys)

sboxsum :: IO (Circuit ArithGate)
sboxsum = buildCircuitT $ do
        ks <- lift $ randKeyIO 8
        xs <- inputs 8
        ys <- subByte xs
        zs <- zipWithM circXor ys =<< secrets ks
        output =<< circXors zs

xor :: Circuit ArithGate
xor = buildCircuit $ do
    x <- input
    y <- input
    z <- circXor x y
    output z

toState :: [Ref] -> [[[Ref]]]
toState = transpose . safeChunksOf 4 . safeChunksOf 8

fromState :: [[[Ref]]] -> [Ref]
fromState = concat . concat . transpose

-- assume inputs come in chunks of 8
gf28DotProduct :: (Gate g, Monad m) => Circuit g -> Circuit g -> [Int] -> [[Ref]] -> BuilderT g m [Ref]
gf28DotProduct double triple xs ys = do
    when (length xs /= length ys) $ error "[gf28DotProduct] unequal length vectors"
    let mult (1,x) = return x
        mult (2,x) = subcircuit double x
        mult (3,x) = subcircuit triple x
        mult (_,_) = error "whoops"
    ws <- mapM mult (zip xs ys)
    mapM circXors (transpose ws)

gf28VectorMult :: (Gate g, Monad m) => Circuit g -> Circuit g -> [Int] -> [[[Ref]]] -> BuilderT g m [[Ref]]
gf28VectorMult double triple v ms = mapM (gf28DotProduct double triple v) ms

gf28MatrixMult :: (Gate g, Monad m) => Circuit g -> Circuit g -> [[Int]] -> [[[Ref]]] -> BuilderT g m [[[Ref]]]
gf28MatrixMult double triple xs ys = mapM (\x -> gf28VectorMult double triple x (transpose ys)) xs

rotate :: Int -> [a] -> [a]
rotate n xs = drop n xs ++ take n xs

buildLinearParts :: IO Acirc
buildLinearParts = buildCircuitT $ do
    double <- lift $ Acirc.read "optimized_circuits/gf28Double.c2v.acirc"
    triple <- lift $ Acirc.read "optimized_circuits/gf28Triple.c2v.acirc"
    xs <- shiftRows <$> toState <$> inputs 128
    ys <- gf28MatrixMult double triple m xs
    outputs (fromState ys)
  where
    m = [[2, 3, 1, 1],
         [1, 2, 3, 1],
         [1, 1, 2, 3],
         [3, 1, 1, 2]]

buildLinearPartsA2 :: IO Acirc2
buildLinearPartsA2 = buildCircuitT $ do
    double <- lift $ Acirc2.read "optimized_circuits/gf28Double.c2v.acirc2"
    triple <- lift $ Acirc2.read "optimized_circuits/gf28Triple.c2v.acirc2"
    xs <- shiftRows <$> toState <$> inputs 128
    ys <- gf28MatrixMult double triple m xs
    outputs (fromState ys)
  where
    m = [[2, 3, 1, 1],
         [1, 2, 3, 1],
         [1, 1, 2, 3],
         [3, 1, 1, 2]]

shiftRows :: [[[Ref]]] -> [[[Ref]]]
shiftRows xs = [ rotate n row | row <- xs | n <- [0..3] ]
