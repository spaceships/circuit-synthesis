{-# LANGUAGE TupleSections #-}

module Examples.Garbler where

import Circuit
import Circuit.Builder
import Circuit.Builder.Internals (runCircuitT)
import Circuit.Conversion
import Circuit.Utils
import Examples.Goldreich
import Examples.Simple

import Control.Monad
import Control.Monad.Trans
import Data.Array
import Lens.Micro.Platform
import System.IO
import Text.Printf
import qualified Data.IntMap as IM

--------------------------------------------------------------------------------
-- global constants

securityParam = 80 -- security parameter, wirelabel & prg seed size
paddingSize   = 10 -- length of the padding in the garbled tables

--------------------------------------------------------------------------------

export :: [(String, [IO (String, Acirc2)])]
export =
    [ ("garbled-andn",  [ garblerQuery ] )
    , ("igarbled-andn", [ indexedGarblerQuery ] )

    , ("garbled-ands", [ ("garbled_and1",)   . view _1 <$> naiveGarbler 1 (andCirc 1)
                       , ("garbled_and10",)  . view _1 <$> naiveGarbler 1 (andCirc 10)
                       , ("garbled_and100",) . view _1 <$> naiveGarbler 1 (andCirc 100)
                       ] )

    , ("igarbled-ands", [ ("igarbled_and1",)   . view _1 <$> indexedGarbler 1 1 (andCirc 1)
                        , ("igarbled_and10",)  . view _1 <$> indexedGarbler 1 1 (andCirc 10)
                        , ("igarbled_and100",) . view _1 <$> indexedGarbler 1 1 (andCirc 100)
                        ] )
    ]
  where
    garblerQuery = do
        putStr "How many and-gates? " >> hFlush stdout
        nands <- read <$> getLine
        putStr "How many seeds? " >> hFlush stdout
        nseeds <- read <$> getLine
        let s = printf "garbler_and%d_s%d" nands nseeds
        c <- view _1 <$> naiveGarbler nseeds (andCirc nands)
        return (s,c)

    indexedGarblerQuery = do
        putStr "How many and-gates? " >> hFlush stdout
        nands <- read <$> getLine
        putStr "How many seeds? " >> hFlush stdout
        nseeds <- read <$> getLine
        putStr "How many indices? " >> hFlush stdout
        nindices <- read <$> getLine
        let s = printf "igarbler_and%d_s%d_i%d" nands nseeds nindices
        c <- view _1 <$> indexedGarbler nseeds nindices (andCirc nands)
        return (s,c)

--------------------------------------------------------------------------------
-- a circuit for the garbler of a garbled circuit scheme

-- XXX: only fan-out one is secure at the moment
-- garbler that does not use permutation bits
naiveGarbler :: Int -> Circ2 -> IO (Acirc2, (Circ, Circ))
naiveGarbler nseeds c = runCircuitT $ do
    -- the seed to the PRGs
    s <- foldM1 (zipWithM circAdd) =<< replicateM nseeds (symbol securityParam)

    (g1, g1Save) <- do
        g <- prgBuilder securityParam (2*securityParam*nwires c) 5 xorAnd -- prg for generating wires
        let g' xs = pairsOf . safeChunksOf securityParam <$> g xs
        asCirc <- lift $ buildCircuitT (inputs securityParam >>= g >>= outputs)
        return (g', toCirc asCirc)

    (g2, g2Save) <- do
        g <- prgBuilder securityParam (2*(securityParam+paddingSize)) 5 xorAnd
        let g' i xs = (!! i) . safeChunksOf (securityParam+paddingSize) <$> g xs
        asCirc <- lift $ buildCircuitT (inputs securityParam >>= g >>= outputs)
        return (g', toCirc asCirc)

    wires <- IM.fromList . (zip (map getRef (wireRefs c))) <$> g1 s

    let pairs = (map.map) ((wires IM.!) . getRef) (map gateArgs (map snd (gates c)))
        xPairs = map head pairs
        yPairs = map last pairs

    -- plaintext outputs for the output gates
    one  <- constant 1
    zero <- constant 0

    let pad = replicate paddingSize zero

    gs <- forM (zip [0..] (gates c)) $ \(i, (zref, g)) -> do
        let [xref, yref] = gateArgs g
            (x0, x1) = wires IM.! getRef xref
            (y0, y1) = wires IM.! getRef yref
            (z0, z1) = wires IM.! getRef zref

        let xwire i = if i == 0 then x0 else x1
            ywire i = if i == 0 then y0 else y1
            zwire i = pad ++ (if i == 0 then z0 else z1)
            zOutWire i = pad ++ replicate (securityParam-1) zero ++ [if i == 0 then zero else one]

        tt <- lift $ randIO (randomize (permutations 2 [0,1]))

        forM tt $ \[i,j] -> do
            mx <- g2 j (xwire i)
            my <- g2 i (ywire j)
            let zf = if isOutputRef c zref then zOutWire else zwire
            foldM1 (zipWithM circXor) [mx, my, zf (gateEval (const undefined) g [i,j])]

    outputs $ (concat.concat) gs
    return (g1Save, g2Save)


-- XXX: only fan-out one is secure at the moment
-- garbler that does not use permutation bits
indexedGarbler :: Int -> Int -> Circ2 -> IO (Acirc2, (Circ, Circ))
indexedGarbler nseeds nindices c = runCircuitT $ do
    -- params
    let ixLen = ceiling (fromIntegral (ngates c) ** (1 / fromIntegral nindices))

    -- the seed to the PRGs, as nseeds different inputs
    s  <- foldM1 (zipWithM circAdd) =<< replicateM nseeds (symbol securityParam)
    ix <- sigmaProd =<< replicateM nindices (sigma ixLen) -- the index of the gate to evaluate

    (g1, g1Save) <- do
        g <- prgBuilder securityParam (2*securityParam*nwires c) 5 xorAnd
        let g' xs = safeChunksOf (2*securityParam) <$> g xs
        asCirc <- lift $ buildCircuitT (inputs securityParam >>= g >>= outputs)
        return (g', toCirc asCirc)

    (g2, g2Save) <- do
        g <- prgBuilder securityParam (2*(securityParam+paddingSize)) 5 xorAnd
        let g' i xs = (!! i) . safeChunksOf (securityParam+paddingSize) <$> g xs
        asCirc <- lift $ buildCircuitT (inputs securityParam >>= g >>= outputs)
        return (g', toCirc asCirc)

    allWires <- listArray (0, nwires c - 1) <$> g1 s
    (mapM . mapM) markPersistant allWires

    -- plaintext outputs for the output gates
    outWires <- do
        one  <- constant 1
        zero <- constant 0
        let zs = replicate (securityParam-1) zero
        return [zs++[zero], zs++[one]]

    -- gate wires is a list of lists of the wires needed for the ith garbled gate, in the correct order
    gateWires <- lift $ randIO $ forM (gates c) $ \(zref,g) -> do
        let [xref,yref] = gateArgs g
            xs = safeChunksOf securityParam $ allWires ! getRef xref
            ys = safeChunksOf securityParam $ allWires ! getRef yref
            zs = safeChunksOf securityParam $ allWires ! getRef zref

            get stuff 0 = head stuff
            get stuff 1 = last stuff

        -- randomize the truth table for gate g
        tt <- randomize (permutations 2 [0,1])
        -- for each table entry, we need to know whether to encrypt the first or second wirelabel of z
        fmap concat $ forM tt $ \[i,j] -> do
            let x = get xs i
                y = get ys j
                z = if isOutputRef c zref
                       then get outWires (gateEval (const undefined) g [i,j])
                       else get zs (gateEval (const undefined) g [i,j])
            return (x ++ y ++ z)

    -- select the wires for the ith gate
    ws <- safeChunksOf 3 <$> safeChunksOf securityParam <$> selectListSigma ix gateWires

    pad <- constants (replicate paddingSize 0)

    garbledRows <- forM (zip ws (permutations 2 [0,1])) $ \([x,y,z],[i,j]) -> do

        mx <- g2 j x
        my <- g2 i y

        foldM1 (zipWithM circXor) [mx, my, pad ++ z]

    outputs (concat garbledRows)

    return (g1Save, g2Save) -- the PRG for evaluation
