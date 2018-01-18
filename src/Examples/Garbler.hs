{-# LANGUAGE TupleSections #-}

module Examples.Garbler where

import Circuit
import Circuit.Builder
import Circuit.Utils
import Circuit.Conversion
import Examples.Goldreich
import Examples.Simple

import Control.Monad
import Control.Monad.Trans
import Data.Array
import System.IO
import Text.Printf
import qualified Data.IntMap as IM

export :: [(String, [IO (String, Acirc2)])]
export =
    [ ("garbled-andn",  [ garblerQuery ] )
    , ("igarbled-andn", [ indexedGarblerQuery ] )

    , ("garbled-ands", [ ("garbled_and1.acirc2",)   <$> naiveGarbler 1 (andCirc 1)
                       , ("garbled_and10.acirc2",)  <$> naiveGarbler 1 (andCirc 10)
                       , ("garbled_and100.acirc2",) <$> naiveGarbler 1 (andCirc 100)
                       ] )

    , ("igarbled-ands", [ ("igarbled_and1.acirc2",)   <$> indexedGarbler 1 1 (andCirc 1)
                        , ("igarbled_and10.acirc2",)  <$> indexedGarbler 1 1 (andCirc 10)
                        , ("igarbled_and100.acirc2",) <$> indexedGarbler 1 1 (andCirc 100)
                        ] )
    ]
  where
    garblerQuery = do
        putStr "How many and-gates? " >> hFlush stdout
        nands <- read <$> getLine
        putStr "How many seeds? " >> hFlush stdout
        nseeds <- read <$> getLine
        let s = printf "garbler_and%d_s%d.acirc2" nands nseeds
        c <- naiveGarbler nseeds (andCirc nands)
        return (s,c)

    indexedGarblerQuery = do
        putStr "How many and-gates? " >> hFlush stdout
        nands <- read <$> getLine
        putStr "How many seeds? " >> hFlush stdout
        nseeds <- read <$> getLine
        putStr "How many indices? " >> hFlush stdout
        nindices <- read <$> getLine
        let s = printf "igarbler_and%d_s%d_i%d.acirc2" nands nseeds nindices
        c <- indexedGarbler nseeds nindices (andCirc nands)
        return (s,c)

--------------------------------------------------------------------------------
-- a circuit for the garbler of a garbled circuit scheme

-- XXX: only fan-out one is secure at the moment
-- garbler that does not use permutation bits
naiveGarbler :: Int -> Circ -> IO Acirc2
naiveGarbler nseeds c' = buildCircuitT $ do
    let c = toCirc2 c'
        k = 80 -- security parameter, wirelabel & prg seed size
        paddingSize = 10

    -- the seed to the PRGs
    s <- foldM1 (zipWithM circAdd) =<< replicateM nseeds (symbol k)

    (g1, g1Desc) <- do
        (g,s) <- prgBuilder' k (2*k*nwires c) 5 xorAnd -- prg for generating wires
        let g' xs = pairsOf . safeChunksOf k <$> g xs
        return (g',s)

    (g2, g2Desc) <- do
        (g,s) <- prgBuilder' k (2*(k+paddingSize)) 5 xorAnd
        let g' i xs = (!! i) . safeChunksOf (k+paddingSize) <$> g xs
        return (g',s)

    lift $ writeFile "g1.txt" g1Desc
    lift $ writeFile "g2.txt" g2Desc

    wires <- IM.fromList . (zip (map getRef (wireRefs c))) <$> g1 s

    let pairs = (map.map) ((wires IM.!) . getRef) (map gateArgs (map snd (gates c)))
        xPairs = map head pairs
        yPairs = map last pairs

    pad <- constants (replicate paddingSize 0)

    gs <- forM (zip [0..] (gates c)) $ \(i, (zref, g)) -> do
        let [xref, yref] = gateArgs g
            (x0, x1) = wires IM.! getRef xref
            (y0, y1) = wires IM.! getRef yref
            (z0, z1) = wires IM.! getRef zref

        let xwire i = if i == 0 then x0 else x1
            ywire i = if i == 0 then y0 else y1
            zwire i = (if i == 0 then z0 else z1) ++ pad

        tt <- lift $ randIO (randomize (permutations 2 [0,1]))

        forM (zip (permutations 2 [0,1]) tt) $ \([i,j], [i',j']) -> do
            mx <- g2 j (xwire i)
            my <- g2 i (ywire j)
            foldM1 (zipWithM circXor) [mx, my, zwire (eval g i' j')]

    outputs $ (concat.concat) gs

  where
    eval g x y = gateEval (\_ -> error "FOO") (\_ -> error "BAR") g [fromIntegral x, fromIntegral y]


-- XXX: only fan-out one is secure at the moment
-- garbler that does not use permutation bits
indexedGarbler :: Int -> Int -> Circ -> IO Acirc2
indexedGarbler nseeds nindices c' = buildCircuitT $ do
    let c = toCirc2 c' -- fold NOT gates into AND and XOR

    -- params
    let k = 80 -- security parameter, wirelabel & prg seed size
        paddingSize = 10 -- length of the padding in the garbled tables
        ixLen = ceiling (fromIntegral (ngates c) ** (1 / fromIntegral nindices))

    -- the seed to the PRGs, as nseeds different inputs
    s  <- foldM1 (zipWithM circAdd) =<< replicateM nseeds (symbol k)
    ix <- sigmaProd =<< replicateM nindices (sigma ixLen) -- the index of the gate to evaluate

    g1 <- do
        g <- prgBuilder k (2*k*nwires c) 5 xorAnd
        return $ \xs -> safeChunksOf (2*k) <$> g xs

    (g2, g2Desc) <- do
        (g,s) <- prgBuilder' k (2*(k+paddingSize)) 5 xorAnd
        let g' i xs = (!! i) . safeChunksOf (k+paddingSize) <$> g xs
        return (g',s)

    -- lift $ writeFile "g1.txt" g1Desc
    -- lift $ writeFile "g2.txt" g2Desc

    allWires <- listArray (0, nwires c - 1) <$> g1 s
    (mapM . mapM) markPersistant allWires

    -- gate wires is a list of lists of the wires needed for the ith garbled gate, in the correct order
    gateWires <- lift $ randIO $ forM (gates c) $ \(zref,g) -> do
        let [xref,yref] = gateArgs g
            xs = safeChunksOf k $ allWires ! getRef xref
            ys = safeChunksOf k $ allWires ! getRef yref
            zs = safeChunksOf k $ allWires ! getRef zref

            get stuff 0 = head stuff
            get stuff 1 = last stuff

        -- randomize the truth table for gate g
        tt <- randomize (permutations 2 [0,1])
        -- for each table entry, we need to know whether to encrypt the first or second wirelabel of z
        fmap concat $ forM tt $ \[i,j] -> do
            let x = get xs i
                y = get ys j
                z = get zs (eval g i j)
            return (x ++ y ++ z)

    -- select the wires for the ith gate
    ws <- safeChunksOf 3 <$> safeChunksOf k <$> selectListSigma ix gateWires

    pad <- constants (replicate paddingSize 0)

    garbledRows <- forM (zip ws (permutations 2 [0,1])) $ \([x,y,z],[i,j]) -> do

        mx <- g2 j x
        my <- g2 i y

        foldM1 (zipWithM circXor) [mx, my, z ++ pad]

    outputs (concat garbledRows)

  where
    eval g x y = gateEval (\_ -> error "FOO") (\_ -> error "BAR") g [fromIntegral x, fromIntegral y]
