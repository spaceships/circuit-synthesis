{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Strict #-}

import Circuit
import Circuit.Builder
import qualified Circuit.Format.Acirc as Acirc
import Util

import Control.Monad
import Data.List.Split
import Debug.Trace
import qualified Data.Vector as V

--------------------------------------------------------------------------------
-- builders

majorityNaive :: [Ref] -> Builder Ref
majorityNaive xs = do
    let cs = combinations (length xs `div` 2) xs
    zs <- mapM circProd cs
    circOrs zs

majority :: [Ref] -> Builder Ref
majority xs = do
    sel <- subcircuit (toRachel n) xs
    let vars = snd <$> filter (\(i,_) -> tt V.! i) (zip [0..] sel)
    circSum vars
  where
    n = length xs
    tt = maj <$> sequence (replicate n [False, True])
    maj xs = sum (map b2i xs) >= (n `div` 2)

xorMaj :: [Ref] -> Builder Ref
xorMaj xs = do
    let n = length xs `div` 2
    wl <- circXors (take n xs)
    {-wr <- majorityNaive (drop n xs)-}
    wr <- majority (drop n xs)
    circXor wl wr

toRachel :: Int -> Circuit
toRachel n = buildCircuit $ do
    xs  <- inputs n
    one <- constant 1
    let vals = sequence (replicate n [False, True])
    zs  <- mapM (bitsSet one xs) vals
    outputs zs

bitsSet :: Ref -> [Ref] -> [Bool] -> Builder Ref
bitsSet one xs bs = do
    when (length xs /= length bs) $ error "[bitsSet] unequal length inputs"
    let set one (x, True)  = return x
        set one (x, False) = circSub one x
    zs <- mapM (set one) (zip xs bs)
    circProd zs

-- l is the size of the index, not the value
select :: Int -> Circuit
select l = buildCircuit $ do
    val <- inputs (2^l)
    ix  <- inputs l
    sel <- subcircuit (toRachel l) ix
    zs  <- zipWithM (circMul) sel val
    z   <- circSum zs
    output z

selects :: Int -> Int -> Circuit
selects m l = buildCircuit $ do
    val <- inputs (2^l)
    zs  <- replicateM m $ do
        ix <- inputs l
        subcircuit (select l) (val ++ ix)
    outputs (concat zs)

f1 :: Int -> Int -> IO Circuit
f1 n m = do
    keyBits <- randKeyIO n
    return $ buildCircuit $ do
        let l = ceiling (logBase 2 (fromIntegral n))
            d = l
        key <- secrets keyBits
        zs  <- replicateM m $ do
            xs <- inputs (d * l)
            bs <- subcircuit (selects d l) (key ++ xs)
            xorMaj bs
        outputs zs

maj8n :: Circuit
maj8n = buildCircuit (output =<< majorityNaive =<< inputs 8)

maj8 :: Circuit
maj8 = buildCircuit (output =<< majority =<< inputs 8)

xormaj16 :: Circuit
xormaj16 = buildCircuit (output =<< xorMaj =<< inputs 16)

f1_128 :: IO Circuit
f1_128 = f1 128 1
