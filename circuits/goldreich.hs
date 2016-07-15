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

majority :: [Ref] -> Builder Ref
majority xs = do
    let cs = combinations (length xs `div` 2) xs
    zs <- mapM circProd cs
    circOrs zs

-- TODO: turn this into a table and use toRachael?
xorMaj :: [Ref] -> Builder Ref
xorMaj xs = do
    let n = length xs `div` 2
    wl <- circXors (take n xs)
    wr <- majority (drop n xs)
    circXor wl wr

toRachel :: Int -> Circuit
toRachel n = buildCircuit $ do
    xs  <- inputs n
    one <- constant 1
    let vals = sequence (replicate n [False, True])
    zs  <- mapM (bitsSet one xs) vals
    outputs zs
  where
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

maj8 :: Circuit
maj8 = buildCircuit (output =<< majority =<< inputs 8)

xormaj16 :: Circuit
xormaj16 = buildCircuit (output =<< xorMaj =<< inputs 16)

f1_128 :: IO Circuit
f1_128 = f1 128 1
