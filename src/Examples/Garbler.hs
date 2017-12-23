{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE Strict #-}
#endif

module Examples.Garbler where

import Circuit
import Circuit.Builder
import Circuit.Utils
import Examples.Goldreich

import Control.Monad
import Data.List.Split
import qualified Data.IntMap as IntMap

makeSizeTest :: IO [(Maybe String, Acirc2)]
makeSizeTest = sequence
    [ (Just "size_test.acirc",) <$> sizeTest ]

--------------------------------------------------------------------------------
-- a circuit for the garbler of a garbled circuit scheme

garbler :: Circ -> IO Acirc2
garbler c = buildCircuitT $ do
    let k = 80 -- security parameter

    s <- inputs k -- the seed to the PRGs

    let g1 = fmap (chunksOf k) . prgBuilder k (2 * ngates c * k) 5 xorAnd -- prg for generating wires
    -- let g2 = fmap (chunksOf k) . prgBuilder k (2*k) 5 xorAnd -- prg for encrypting table entries

    -- generate pairs of wirelabels for every wire in c
    rawWires  <- pairsOf <$> g1 s
    withPbits <- forM rawWires $ \(xs,ys) -> do
        -- set the permute bit of X1 according to the lsb of X0
        z <- circNot (last xs)
        return (xs, init ys ++ [z])
    let wires = IntMap.fromList (zip [0..] withPbits) -- indexed by the refs of c

    -- generate garbled tables for every gate in c
    tabs <- forM (gates c) $ \(zref, g) -> do
        let [xref, yref] = gateArgs g
            x = wires IntMap.! getRef xref
            y = wires IntMap.! getRef yref
            z = wires IntMap.! getRef zref

        -- let g = case op of { (OpMul _ _) -> (&&); _ -> xor }

            -- gx <- g2 (x!!i)
            -- gy <- g2 (y!!j)

            -- unpermuted_gate <- forM (replicateM 2 [0,1] :: [[Int]]) $ \[i,j] -> do
            -- foldM1 (zipWithM circXor) [gate_x, gate_y, z !! b2i (i2b i && i2b j)]
        undefined



    -- let x = [x0,x1]
    --     y = [y0,y1]
    --     z = [z0,z1]

    -- unpermuted_gate <- forM (replicateM 2 [0,1] :: [[Int]]) $ \[i,j] -> do
    --     gate_x <- g2 (x!!i)
    --     gate_y <- g2 (y!!j)
    --     foldM1 (zipWithM circXor) [gate_x, gate_y, z !! b2i (i2b i && i2b j)]

    -- -- generate permute bits
    -- [b0,b1] <- g3 s

    -- -- swap the ys based on b1
    -- p0 <- swap b1 (unpermuted_gate !! 0) (unpermuted_gate !! 1)
    -- p1 <- swap b1 (unpermuted_gate !! 2) (unpermuted_gate !! 3)
    -- let partially_permuted_gate = p0 ++ p1

    -- -- swap the xs based on b0
    -- p3 <- swap b0 (concat (take 2 partially_permuted_gate)) (concat (drop 2 partially_permuted_gate))
    -- let permuted_gate = concatMap (chunksOf k) p3

    -- outputs (concat permuted_gate)
    undefined

--------------------------------------------------------------------------------
-- test to see how well we can evaluate extra large circuits

sizeTest :: IO Acirc2
sizeTest = buildCircuitT $ do
    let n = 80
    xs <- inputs n
    let g1 = prgBuilder n (1000*n) (numBits n) xorAnd
    let g2 = prgBuilder n n (numBits n) xorAnd
    ys <- chunksOf n <$> g1 xs
    z0 <- mapM g2 ys
    z1 <- zipWithM (zipWithM circMul) z0 ys
    z2 <- zipWithM (zipWithM circMul) z1 ys
    zs <- foldM1 (zipWithM circXor) z2
    outputs zs
