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
    [ ("garble_and2.acirc", garbler (andCirc 2))
    , ("garble_and3.acirc", garbler (andCirc 3))
    , ("garble_and4.acirc", garbler (andCirc 4))
    ]

--------------------------------------------------------------------------------
-- a circuit for the garbler of a garbled circuit scheme

-- XXX: only fan out one is secure at the moment
garbler :: Circ -> IO Acirc2
garbler c = buildCircuitT $ do
    let k = 80 -- security parameter, wirelabel & prg seed size

    s  <- inputs k -- the seed to the PRGs
    ix <- inputs (nwires c) -- the gate index in sigma vector form

    let numWirelabelsToGenerate = 2*(nwires c - noutputs c) -- the output wires get the actual value

    g1 <- do
        g <- prgBuilder k (numWirelabelsToGenerate * (k+1)) 5 xorAnd -- prg for generating wires
        let g' xs = safeChunksOf (k+1) <$> g xs
        return g'

    g2 <- do
        g <- prgBuilder k (2*(k+1)) 5 xorAnd
        let g' i xs = (!! i) . safeChunksOf (k+1) <$> g xs
        return g'

    -- generate pairs of wirelabels for every intermediate wire in c
    bits <- pairsOf <$> g1 s
    withPbits <- forM bits $ \(w0,w1) -> do
        p1 <- circNot (head w0)
        return (w0, p1:tail w1) -- permuation bit is FIRST bit of wirelabels
    let intermediateWirelabels = IntMap.fromList (zip (map getRef (intermediateWireRefs c)) withPbits)

    -- output wirelabels are fixed here, so we dont have to branch in garble
    -- this makes indexing easier since there is only one routine for garbling
    zero <- mapM constant (replicate (k+1) 0)
    one  <- mapM constant (1 : replicate k 0) -- truth value is FIRST bit of output wirelabels
    let outputWirelabels = IntMap.fromList (zip (map getRef (outputRefs c)) (repeat (zero, one)))
        wires = IntMap.union intermediateWirelabels outputWirelabels

    -- TODO: index the wires so we dont have to loop over anything in the garbler.
    -- TODO: annotate circuit: anything before this point will be reusable.

    -- generate garbled tables for every gate in c
    tabs <- forM (gates c) $ \(zref,g) -> do

        let [xref, yref] = gateArgs g
            ((px:x0), (_:x1)) = wires IntMap.! getRef xref
            ((py:y0), (_:y1)) = wires IntMap.! getRef yref
            (z0, z1)          = wires IntMap.! getRef zref

            xwire i = if i == 0 then x0 else z1
            ywire i = if i == 0 then y0 else y1
            zwire i = if i == 0 then z0 else z1

        [g0,g1,g2,g3] <- forM (permutations 2 [0,1]) $ \[i,j] -> do
            mx <- g2 j (xwire i)
            my <- g2 i (ywire j)
            foldM1 (zipWithM circXor) [mx, my, zwire (eval g i j)]

        -- swap the ys based on p1
        h0 <- swap py g0 g1
        h1 <- swap py g2 g3

        -- swap the xs based on p0
        h3 <- swap px (concat h0) (concat h1)
        let permutedGate = concatMap (safeChunksOf (k+1)) h3 :: [[Ref]]
        return permutedGate

    outputs ((concat.concat) tabs)
  where
    eval g x y = fromIntegral (gateEval (const $ error "FOO") (const $ error "BAR") g [fromIntegral x, fromIntegral y])

--------------------------------------------------------------------------------
-- simple circuit for testing garble

andCirc :: Int -> Circ
andCirc n = buildCircuit (inputs n >>= circProd >>= output)

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
