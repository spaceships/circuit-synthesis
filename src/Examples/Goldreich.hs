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
import Data.List.Split (chunksOf)

make :: [(String, IO Acirc)]
make =
    [ ("prg_xormaj_32_32.dsl.acirc"   , prg'  32  32 5 xorMaj)
    , ("prg_xormaj_32_128.dsl.acirc"  , prg'  32 128 5 xorMaj)
    , ("prg_xormaj_64_64.dsl.acirc"   , prg'  64  64 6 xorMaj)
    , ("prg_xormaj_64_128.dsl.acirc"  , prg'  64 128 6 xorMaj)
    , ("prg_xormaj_128_128.dsl.acirc" , prg' 128 128 7 xorMaj)

    , ("prg_xorand_32_32.dsl.acirc"   , prg'  32  32 5 xorAnd)
    , ("prg_xorand_32_128.dsl.acirc"  , prg'  32 128 5 xorAnd)
    , ("prg_xorand_64_64.dsl.acirc"   , prg'  64  64 5 xorAnd)
    , ("prg_xorand_64_128.dsl.acirc"  , prg'  64 128 5 xorAnd)
    , ("prg_xorand_128_128.dsl.acirc" , prg' 128 128 5 xorAnd)
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

prgKey :: Gate g => Int -> Int -> IO (Circuit g)
prgKey n m = buildCircuitT $ do
    let l = numBits n
        d = l
    keyBits <- lift $ randKeyIO n
    selections <- lift $ replicateM m $ replicateM d (randIO (randIntegerMod (fromIntegral n)))
    xs  <- secrets keyBits
    zs  <- forM selections $ \s -> xorMaj =<< selectsPT (map fromIntegral s) xs
    outputs zs

--------------------------------------------------------------------------------
-- indexed prg

-- naive version
indexedPrg :: Gate g => Int -> Int -> Int -> IO (Circuit g)
indexedPrg ninputs noutputs outputSize = buildCircuitT $ do
    let prg = prgBuilder ninputs (noutputs * outputSize) 5 xorAnd
    xs <- inputs ninputs
    ix <- inputs (numBits noutputs)
    zs <- chunksOf outputSize <$> prg xs
    outputs =<< selectList zs ix
