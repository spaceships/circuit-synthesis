module Examples.Garbler where

import Circuit
import Circuit.Builder
import Circuit.Utils
import Circuit.Conversion
import Examples.Goldreich

import Control.Monad
import Control.Monad.Trans
import Data.List.Split
import Lens.Micro.Platform
import System.IO
import qualified Data.IntMap as IM

export :: [(String, [(String, IO Acirc)])]
export =
    [ ("size_test", [("size_test.acirc", toAcirc <$> sizeTest )])
    , ("garbled_and", [("garbled_andn.acirc", toAcirc <$> (garbler =<< andCirc <$> query))])
    , ("garbled_and1000", [("garbled_and1000.acirc", toAcirc <$> garbler (andCirc 1000))])
    ]
  where
    query = do
        putStr "How many and-gates? "
        hFlush stdout
        read <$> getLine

--------------------------------------------------------------------------------
-- a circuit for the garbler of a garbled circuit scheme

-- XXX: only fan out one is secure at the moment
garbler :: Circ -> IO Acirc2
garbler c' = buildCircuitT $ do
    let c = toCirc2 c'
        k = 80 -- security parameter, wirelabel & prg seed size
        l = maxFanOut c -- which determines the stretch we need from G2

    s  <- inputs k -- the seed to the PRGs
    ix <- inputs (ngates c) -- the gate index in sigma vector form

    let numWirelabelsToGenerate = 2*(nwires c - noutputs c) -- the output wires get the actual value

    (g1, g1Desc) <- do
        (g,s) <- prgBuilder' k (numWirelabelsToGenerate * (k+1)) 5 xorAnd -- prg for generating wires
        let g' xs = safeChunksOf (k+1) <$> g xs
        return (g',s)

    (g2, g2Desc) <- do
        (g,s) <- prgBuilder' k (2*l*(k+1)) 5 xorAnd
        let g' i xs = (!! i) . safeChunksOf (k+1) <$> g xs
        return (g',s)

    lift $ writeFile "g1.txt" g1Desc
    lift $ writeFile "g2.txt" g2Desc

    -- generate pairs of wirelabels for every intermediate wire in c
    bits <- pairsOf <$> g1 s
    withPbits <- forM bits $ \(w0,w1) -> do
        p1 <- circNot (head w0)
        return (w0, p1:tail w1) -- permuation bit is FIRST bit of wirelabels
    let intermediateWirelabels = IM.fromList (zip (map getRef (intermediateWireRefs c)) withPbits)

    -- output wirelabels are fixed here, so we dont have to branch in garble
    -- this makes indexing easier since there is only one routine for garbling
    zero <- mapM constant (replicate (k+1) 0)
    one  <- mapM constant (1 : replicate k 0) -- truth value is FIRST bit of output wirelabels
    let outputWirelabels = IM.fromList (zip (map getRef (outputRefs c)) (repeat (zero, one)))
        wires = IM.union intermediateWirelabels outputWirelabels

    -- annotate circuit: anything before this point will be reusable
    wires & traverseOf (each.each.each) markPersistant

    -- we index the wires so we dont have to loop over anything in the garbler.
    -- z is the output wire for the ith gate
    let zs = flip map (gates c) $ \(ref, g) ->
            let (z0,z1) = wires IM.! getRef ref
            in flip map (permutations 2 [0,1]) $ \[i,j] ->
                if eval g i j then z0 else z1

    z0 <- selectListSigma ix (map (!!0) zs)
    z1 <- selectListSigma ix (map (!!1) zs)
    z2 <- selectListSigma ix (map (!!2) zs)
    z3 <- selectListSigma ix (map (!!3) zs)

    let zwire 0 0 = z0
        zwire 0 1 = z1
        zwire 1 0 = z2
        zwire 1 1 = z3
        zwire _ _ = error "whoops"

    -- x is the first input wire for the ith gate, y is the second input wire
    let pairs = (map.map) ((wires IM.!) . getRef) (map gateArgs (map snd (gates c)))
        xPairs = map head pairs
        yPairs = map last pairs

    (px:x0) <- selectListSigma ix (map fst xPairs)
    (_ :x1) <- selectListSigma ix (map snd xPairs)
    (py:y0) <- selectListSigma ix (map fst yPairs)
    (_ :y1) <- selectListSigma ix (map snd yPairs)

    let xwire i = if i == 0 then x0 else x1
        ywire i = if i == 0 then y0 else y1

    [g0,g1,g2,g3] <- forM (permutations 2 [0,1]) $ \[i,j] -> do
        mx <- g2 j (xwire i)
        my <- g2 i (ywire j)
        foldM1 (zipWithM circXor) [mx, my, zwire i j]

    -- swap the ys based on p1
    h0 <- swap py g0 g1
    h1 <- swap py g2 g3

    -- swap the xs based on p0
    h3 <- swap px (concat h0) (concat h1)
    let permutedGate = concatMap (safeChunksOf (k+1)) h3 :: [[Ref]]
    outputs (concat permutedGate)

  where
    eval g x y = gateEval (\_ -> error "FOO") (\_ -> error "BAR") g [fromIntegral x, fromIntegral y] == 1

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

simple :: Acirc
simple = buildCircuit $ do
    xs  <- inputs 2
    ys  <- secrets [0,1]
    one <- constant 1
    w   <- circProd =<< zipWithM circAdd xs ys
    z   <- circAdd w one
    output z
