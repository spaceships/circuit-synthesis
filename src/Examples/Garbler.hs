module Examples.Garbler where

import Circuit
import Circuit.Builder
import Circuit.Utils
import Circuit.Conversion
import Examples.Goldreich
import Examples.Simple

import Control.Monad
import Control.Monad.Trans
import System.IO
-- import Lens.Micro.Platform
import qualified Data.IntMap as IM

export :: [(String, [(String, IO Acirc2)])]
export =
    [ ("garbled_andn", [("garbled_andn.acirc2", garblerNoPBits =<< andCirc <$> query)])
    , ("garbled_and1000", [("garbled_and1000.acirc2", garblerNoPBits (andCirc 1000))])
    ]
  where
    query = do
        putStr "How many and-gates? "
        hFlush stdout
        read <$> getLine

--------------------------------------------------------------------------------
-- a circuit for the garbler of a garbled circuit scheme

-- XXX: only fan-out one is secure at the moment
-- garbler that does not use permutation bits
garblerNoPBits :: Circ -> IO Acirc2
garblerNoPBits c' = buildCircuitT $ do
    let c = toCirc2 c'
        k = 80 -- security parameter, wirelabel & prg seed size
        paddingSize = 10

    s <- symbol k -- the seed to the PRGs

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
        let [xref, yref]      = gateArgs g
            (x0, x1) = wires IM.! getRef xref
            (y0, y1) = wires IM.! getRef yref
            (z0, z1) = wires IM.! getRef zref

        let xwire i = if i == 0 then x0 else x1
            ywire i = if i == 0 then y0 else y1
            zwire i = (if i == 0 then z0 else z1) ++ pad

        forM (permutations 2 [0,1]) $ \[i,j] -> do
            mx <- g2 j (xwire i)
            my <- g2 i (ywire j)
            foldM1 (zipWithM circXor) [mx, my, zwire (eval g i j)]

    gss <- lift $ randIO $ mapM randomize gs

    outputs $ (concat.concat) gss

  where
    eval g x y = gateEval (\_ -> error "FOO") (\_ -> error "BAR") g [fromIntegral x, fromIntegral y]


-- XXX: only fan-out one is secure at the moment
-- garbler that does not use permutation bits
indexedGarblerNoPBits :: Circ -> IO Acirc2
indexedGarblerNoPBits c' = buildCircuitT $ do
    let c = toCirc2 c'
        k = 80 -- security parameter, wirelabel & prg seed size
        paddingSize = 10

    s <- symbol k -- the seed to the PRGs

    ix <- sigma (ngates c)

    g1 <- indexedPrgSigmaBuilder k (nwires c) (2*k)

    (g2, g2Desc) <- do
        (g,s) <- prgBuilder' k (2*(k+paddingSize)) 5 xorAnd
        let g' i xs = (!! i) . safeChunksOf (k+paddingSize) <$> g xs
        return (g',s)

    -- lift $ writeFile "g1.txt" g1Desc
    -- lift $ writeFile "g2.txt" g2Desc

    -- turn a selection vector for z into a selection vector for x and y
    inputSels <- forM (gates c) $ \(_,g) -> do
        let [xref,yref] = gateArgs g
        xsel <- selectionVectorInt (getRef xref) (nwires c) -- selection vector for x of gate g
        ysel <- selectionVectorInt (getRef yref) (nwires c) -- selection vector for y of gate g
        return (xsel, ysel)

    xsel  <- selectListSigma ix (map fst inputSels) -- select the ith selection vector!
    ysel  <- selectListSigma ix (map snd inputSels)

    x <- g1 s xsel
    y <- g1 s ysel

    -- compute the z wires for every gate, then select the appropriate ones
    -- zsels <- forM (gates c) $ \(_,g) -> do



    -- z <- g1 s zsel

    undefined

    -- let pairs = (map.map) ((wires IM.!) . getRef) (map gateArgs (map snd (gates c)))
    --     xPairs = map head pairs
    --     yPairs = map last pairs

    -- pad <- constants (replicate paddingSize 0)

    -- gs <- forM (zip [0..] (gates c)) $ \(i, (zref, g)) -> do
    --     let [xref, yref]      = gateArgs g
    --         (x0, x1) = wires IM.! getRef xref
    --         (y0, y1) = wires IM.! getRef yref
    --         (z0, z1) = wires IM.! getRef zref

    --     let xwire i = if i == 0 then x0 else x1
    --         ywire i = if i == 0 then y0 else y1
    --         zwire i = (if i == 0 then z0 else z1) ++ pad

    --     forM (permutations 2 [0,1]) $ \[i,j] -> do
    --         mx <- g2 j (xwire i)
    --         my <- g2 i (ywire j)
    --         foldM1 (zipWithM circXor) [mx, my, zwire (eval g i j)]

    -- gss <- lift $ randIO $ mapM randomize gs

    -- outputs $ (concat.concat) gss

  where
    eval g x y = gateEval (\_ -> error "FOO") (\_ -> error "BAR") g [fromIntegral x, fromIntegral y]
