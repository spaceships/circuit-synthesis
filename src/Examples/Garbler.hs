module Examples.Garbler where

import Circuit
import Circuit.Builder
import Circuit.Utils
import Circuit.Conversion
import Examples.Goldreich
import Examples.Simple

import Control.Monad
import Control.Monad.Trans
import Data.List (zip4)
import Lens.Micro.Platform
import System.IO
import qualified Data.IntMap as IM

import Debug.Trace


export :: [(String, [(String, IO Acirc2)])]
export =
    [ ("garbled_andn", [("garbled_andn.acirc2", garblerNoPBits =<< andCirc <$> query)])

    , ("igarbled_andn", [("igarbled_andn.acirc2", indexedGarblerNoPBits =<< andCirc <$> query)])

    , ("garbled_ands", [("garbled_and1.acirc2", garblerNoPBits (andCirc 1))
                       ,("garbled_and10.acirc2", garblerNoPBits (andCirc 10))
                       ,("garbled_and100.acirc2", garblerNoPBits (andCirc 100))
                       ])

    , ("igarbled_ands", [("igarbled_and1.acirc2", indexedGarblerNoPBits (andCirc 1))
                        ,("igarbled_and10.acirc2", indexedGarblerNoPBits (andCirc 10))
                        ,("igarbled_and100.acirc2", indexedGarblerNoPBits (andCirc 100))
                        ])
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


-- TODO: switch to naive indexed PRG
-- XXX: only fan-out one is secure at the moment
-- garbler that does not use permutation bits
indexedGarblerNoPBits :: Circ -> IO Acirc2
indexedGarblerNoPBits c' = buildCircuitT $ do
    let c = toCirc2 c'
        k = 80 -- security parameter, wirelabel & prg seed size
        paddingSize = 10

    s <- symbol k -- the seed to the PRGs

    ix <- sigma (ngates c)

    g1 <- do
        g <- indexedPrgSigmaBuilder k (nwires c) (2*k)
        return $ \xs ix -> safeChunksOf k <$> g xs ix

    (g2, g2Desc) <- do
        (g,s) <- prgBuilder' k (2*(k+paddingSize)) 5 xorAnd
        let g' i xs = (!! i) . safeChunksOf (k+paddingSize) <$> g xs
        return (g',s)

    -- lift $ writeFile "g1.txt" g1Desc
    -- lift $ writeFile "g2.txt" g2Desc

    -- turn a selection vector for the gate into a selection vector for x and y and z
    sels <- forM (gates c) $ \(zref,g) -> do
        let [xref,yref] = gateArgs g
        xsel <- selectionVectorInt (getRef xref) (nwires c) -- selection vector for x of gate g
        ysel <- selectionVectorInt (getRef yref) (nwires c)
        zsel <- selectionVectorInt (getRef zref) (nwires c)

        -- randomize the truth table for gate g
        tt <- lift $ randIO (randomize (permutations 2 [0,1]))
        -- for each table entry, we need to know whether to encrypt the first or second wirelabel of z
        indexSels <- forM tt $ \[i,j] -> do
            -- create length 2 selection vectors for each table entry
            x <- selectionVectorInt i 2
            y <- selectionVectorInt j 2
            z <- selectionVectorInt (eval g i j) 2
            return (x,y,z)

        let xIndexSels = concat $ indexSels ^.. each . _1
            yIndexSels = concat $ indexSels ^.. each . _2
            zIndexSels = concat $ indexSels ^.. each . _3

        return ((xsel, xIndexSels), (ysel, yIndexSels), (zsel, zIndexSels))

    -- the indices for generating the X and Y wirelabels from G1
    xsel  <- selectListSigma ix (sels ^.. each . _1 . _1) -- select the ith selection vector!
    ysel  <- selectListSigma ix (sels ^.. each . _2 . _1)
    zsel  <- selectListSigma ix (sels ^.. each . _3 . _1)

    -- the selections for choosing the 1st or 2nd wirelabel in each garbled row
    xIndexSels <- safeChunksOf 2 <$> selectListSigma ix (sels ^.. each . _1 . _2)
    yIndexSels <- safeChunksOf 2 <$> selectListSigma ix (sels ^.. each . _2 . _2)
    zIndexSels <- safeChunksOf 2 <$> selectListSigma ix (sels ^.. each . _3 . _2)

    -- the wirelabels themselves for gate g
    xwls <- g1 s xsel
    ywls <- g1 s ysel
    zwls <- g1 s zsel

    pad <- constants (replicate paddingSize 0)

    garbledRows <- forM (zip4 xIndexSels yIndexSels zIndexSels (permutations 2 [0,1])) $ \(xix, yix, zix, [i,j]) -> do
        x <- selectListSigma xix xwls
        y <- selectListSigma yix ywls
        z <- selectListSigma zix zwls

        mx <- g2 j x
        my <- g2 i y

        foldM1 (zipWithM circXor) [mx, my, z ++ pad]

    outputs (concat garbledRows)

  where
    eval g x y = gateEval (\_ -> error "FOO") (\_ -> error "BAR") g [fromIntegral x, fromIntegral y]
