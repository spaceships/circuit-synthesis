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

makeGarbledAnd :: [(String, IO Acirc2)]
makeGarbledAnd =
    [ ("garble_and.acirc", garbler andCirc) ]

--------------------------------------------------------------------------------
-- a circuit for the garbler of a garbled circuit scheme

garbler :: Circ -> IO Acirc2
garbler c = buildCircuitT $ do
    let k = 80 -- security parameter

    s  <- inputs k -- the seed to the PRGs
    ix <- inputs (ngates c) -- the gate index in sigma vector form
    -- TODO: index it!

    g1 <- do
        g <- prgBuilder k (2*ngates c*(k+1)) 5 xorAnd -- prg for generating wires
        let g' xs = safeChunksOf (k+1) <$> g xs
        return g'

    g2 <- do
        g <- prgBuilder k (2*(k+1)) 5 xorAnd
        let g' i xs = do ys <- g xs
                         let zs = safeChunksOf (k+1) ys
                         return (zs !! i)
        return g'

    -- generate pairs of wirelabels for every wire in c
    bits <- pairsOf <$> g1 s
    withPbits <- forM bits $ \(w0,w1) -> do
        p1 <- circNot (head w0)
        return (w0, p1:tail w1) -- permuation bit is FIRST bit of wires
    let wires = IntMap.fromList (zip [0..] withPbits) -- indexed by the refs of c

    -- generate garbled tables for every gate in c
    tabs <- forM (nonInputGates c) $ \(zref,g) -> do
        let eval x y = fromIntegral (gateEval (const $ error "FOO") (const $ error "BAR") g [fromIntegral x, fromIntegral y])

            [xref, yref] = gateArgs g
            ((px:x0), (_:x1)) = wires IntMap.! getRef xref
            ((py:y0), (_:y1)) = wires IntMap.! getRef yref
            (z0, z1)          = wires IntMap.! getRef zref

            xwire i = if i == 0 then x0 else z1
            ywire i = if i == 0 then y0 else y1
            zwire i = if i == 0 then z0 else z1

        [g0,g1,g2,g3] <- forM (permutations 2 [0,1]) $ \[i,j] -> do
            mx <- g2 j (xwire i)
            my <- g2 i (ywire j)
            val <- if isOutputRef c zref then do
                       c  <- constant (eval i j)
                       cs <- constants (replicate k 0)
                       return (c:cs)
                   else do
                       return (zwire (eval i j))
            wireXors [mx, my, val]

        -- swap the ys based on p1
        h0 <- swap py g0 g1
        h1 <- swap py g2 g3

        -- swap the xs based on p0
        h3 <- swap px (concat h0) (concat h1)
        let permuted_gate = concatMap (safeChunksOf (k+1)) h3 :: [[Ref]]
        return permuted_gate

    outputs ((concat.concat) tabs)

  where
    wireXors = foldM1 (zipWithM circXor)

--------------------------------------------------------------------------------
-- simple circuit for testing garble

andCirc :: Circ
andCirc = buildCircuit $ do
    x <- input
    y <- input
    output =<< circMul x y

--------------------------------------------------------------------------------
-- test to see how well we can evaluate extra large circuits

sizeTest :: IO Acirc2
sizeTest = buildCircuitT $ do
    let n = 80
    xs <- inputs n
    g1 <- prgBuilder n (1000*n) (numBits n) xorAnd
    g2 <- prgBuilder n n (numBits n) xorAnd
    ys <- chunksOf n <$> g1 xs
    z0 <- mapM g2 ys
    z1 <- zipWithM (zipWithM circMul) z0 ys
    z2 <- zipWithM (zipWithM circMul) z1 ys
    zs <- foldM1 (zipWithM circXor) z2
    outputs zs
