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
import qualified Data.IntMap as IM

export :: [(String, [(String, IO Acirc2)])]
export =
    [ ("garbled_andn", [("garbled_andn.acirc2", garbler =<< andCirc <$> query)])
    , ("garbled_and1000", [("garbled_and1000.acirc2", garbler (andCirc 1000))])
    ]
  where
    query = do
        putStr "How many and-gates? "
        hFlush stdout
        read <$> getLine

--------------------------------------------------------------------------------
-- a circuit for the garbler of a garbled circuit scheme

-- XXX: only fan out one is secure at the moment
-- indexed prg version
indexedGarbler :: Circ -> IO Acirc2
indexedGarbler c' = buildCircuitT $ do
    let c = toCirc2 c'
        k = 80 -- security parameter, wirelabel & prg seed size
        l = maxFanOut c -- which determines the stretch we need from G2

    setSymlen k

    s  <- inputs k -- the seed to the PRGs
    ix <- inputs (ngates c) -- the gate index in sigma vector form

    g1 <- do
        g <- indexedPrgSigmaBuilder k (nwires c) (2*k+1)
        return $ \s ix -> do -- fix the permutation bits
            (p0:rs) <- g s ix
            p1 <- circNot p0
            return [p0 : take k rs, p1 : drop k rs] -- NOTE: the permutation bit is the first bit of each wirelabel

    g2 <- indexedPrgSigmaBuilder k 2 (k+1) -- TODO: support higher fan-out somehow -- more selections!
    g2 <- do
        g <- prgBuilder k (2*(k+1)) 5 xorAnd
        return $ \i xs -> do
            ys <- g xs
            return $ case i of
                0 -> take (k+1) ys
                1 -> drop (k+1) ys
                _ -> error "[g2] weird index!"

    -- turn a selection vector for z into a selection vector for x and y
    inputSels <- forM (gates c) $ \(_,g) -> do
        let [xref,yref] = gateArgs g
        xsel <- selectionVectorInt (getRef xref) (nwires c) -- selection vector for x of gate g
        ysel <- selectionVectorInt (getRef yref) (nwires c) -- selection vector for y of gate g
        return (xsel, ysel)

    xsel <- selectListSigma ix (map fst inputSels) -- select the ith selection vector!
    ysel <- selectListSigma ix (map snd inputSels)

    x <- g1 s xsel
    y <- g1 s ysel
    z <- g1 s ix
    undefined

  --   [g0,g1,g2,g3] <- forM (permutations 2 [0,1]) $ \[i,j] -> do
  --       mx <- g2 j (x !! i)
  --       my <- g2 i (y !! j)
  --       foldM1 (zipWithM circXor) [mx, my, z !! eval g i j]

  --   -- swap the ys based on p1
  --   h0 <- swap py g0 g1
  --   h1 <- swap py g2 g3

  --   -- swap the xs based on p0
  --   h3 <- swap px (concat h0) (concat h1)
  --   let permutedGate = concatMap (safeChunksOf (k+1)) h3 :: [[Ref]]
  --   outputs (concat permutedGate)

  -- where
  --   eval g x y = gateEval (\_ -> error "FOO") (\_ -> error "BAR") g [fromIntegral x, fromIntegral y] == 1

-- XXX: only fan out one is secure at the moment
-- persistant refs version
indexedGarblerWires :: Circ -> IO Acirc2
indexedGarblerWires c' = buildCircuitT $ do
    let c = toCirc2 c'
        k = 80 -- security parameter, wirelabel & prg seed size
        l = maxFanOut c -- which determines the stretch we need from G2

    setSymlen k

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

    -- -- annotate circuit: anything before this point will be reusable
    -- wires & traverseOf (each.each.each) markPersistant

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

-- full garbler
-- XXX: only fan-out one is secure at the moment
garbler :: Circ -> IO Acirc2
garbler c' = buildCircuitT $ do
    let c = toCirc2 c'
        k = 80 -- security parameter, wirelabel & prg seed size

    setSymlen k

    s <- inputs k -- the seed to the PRGs

    (g1, g1Desc) <- do
        (g,s) <- prgBuilder' k (nwires c * (2*k+1)) 5 xorAnd -- prg for generating wires
        let g' xs = do ys <- safeChunksOf (2*k+1) <$> g xs
                       forM ys $ \(p0:w) -> do
                           p1 <- circNot p0
                           return (p0 : take k w, p1 : drop k w)
        return (g',s)

    (g2, g2Desc) <- do
        (g,s) <- prgBuilder' k (2*(k+1)) 5 xorAnd
        let g' i xs = (!! i) . safeChunksOf (k+1) <$> g xs
        return (g',s)

    lift $ writeFile "g1.txt" g1Desc
    lift $ writeFile "g2.txt" g2Desc

    -- intermediateWirelabels <- IM.fromList . (zip (map getRef (intermediateWireRefs c))) <$> g1 s
    wires <- IM.fromList . (zip (map getRef (wireRefs c))) <$> g1 s

    -- output wirelabels are fixed here, so we dont have to branch in garble
    -- XXX the first element should be a secret, rest const 0
    -- zero <- mapM seccrets (replicate (k+1) 0)
    -- one  <- mapM constant (1 : replicate k 0) -- truth value is FIRST bit of output wirelabels
    -- let outputWirelabels = IM.fromList (zip (map getRef (outputRefs c)) (repeat (zero, one)))
    --     wires = IM.union intermediateWirelabels outputWirelabels

    let pairs = (map.map) ((wires IM.!) . getRef) (map gateArgs (map snd (gates c)))
        xPairs = map head pairs
        yPairs = map last pairs

    ggs <- forM (zip [0..] (gates c)) $ \(i, (zref, g)) -> do
        let [xref, yref]      = gateArgs g
            ((px:x0), (_:x1)) = wires IM.! getRef xref
            ((py:y0), (_:y1)) = wires IM.! getRef yref
            (z0, z1)          = wires IM.! getRef zref

        let xwire i = if i == 0 then x0 else x1
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

        return $ concatMap (safeChunksOf (k+1)) h3

    outputs $ (concat.concat) ggs

  where
    eval g x y = gateEval (\_ -> error "FOO") (\_ -> error "BAR") g [fromIntegral x, fromIntegral y]


-- XXX: only fan-out one is secure at the moment
-- garbler that does not use permutation bits
garblerNoPBits :: Circ -> IO Acirc2
garblerNoPBits c' = buildCircuitT $ do
    let c = toCirc2 c'
        k = 80 -- security parameter, wirelabel & prg seed size
        paddingSize = 10

    setSymlen k

    s <- inputs k -- the seed to the PRGs

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
