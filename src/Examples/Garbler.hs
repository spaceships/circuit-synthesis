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

makeSizeTest :: [(String, IO Acirc2)]
makeSizeTest =
    [ ("size_test.acirc", sizeTest ) ]

--------------------------------------------------------------------------------
-- a circuit for the garbler of a garbled circuit scheme

garbler :: Circ -> IO Acirc2
garbler c = buildCircuitT $ do
    let k = 80 -- security parameter

    s  <- inputs k -- the seed to the PRGs
    ix <- inputs (ngates c) -- the gate index

    -- XXX will these create a new PRG for every invocation????
    let g1 = fmap (chunksOf (k+1)) . prgBuilder k (2*ngates c*k) 5 xorAnd -- prg for generating wires
    let g2 i = fmap ((!!i) . chunksOf (k+1)) . prgBuilder k (2*(k+1)) 5 xorAnd -- prg for encrypting table entries

    -- generate pairs of wirelabels for every wire in c
    bits  <- pairsOf <$> g1 s
    withPbits <- forM bits $ \(w0,w1) -> do
        p1 <- circNot (head w0)
        return (w0, p1:tail w1) -- permuation bit is FIRST bit of wires
    let wires = IntMap.fromList (zip [0..] withPbits) -- indexed by the refs of c

    -- generate garbled tables for every gate in c
    tabs <- forM (intermediateGates c) $ \(zref,g) -> do
        let eval x y = fromIntegral (gateEval undefined undefined g [fromIntegral x, fromIntegral y])

            [xref, yref] = gateArgs g
            ((px:x0), (_:x1)) = wires IntMap.! getRef xref
            ((py:y0), (_:y1)) = wires IntMap.! getRef yref
            (z0, z1)          = wires IntMap.! getRef zref

            xwire i = if i == 0 then x0 else z1
            ywire i = if i == 0 then y0 else y1
            zwire i = if i == 0 then z0 else z1

        unpermuted <- forM (permutations 2 [0,1]) $ \[i,j] -> do
            mx <- g2 j (xwire i)
            my <- g2 i (ywire j)
            wireXors [mx, my, zwire (eval 0 0)]

        -- TODO: permute using swaps!!!

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
  where
    wireXors = foldM1 (zipWithM circXor)

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
