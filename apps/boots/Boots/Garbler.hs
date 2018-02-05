{-# LANGUAGE TupleSections #-}

module Boots.Garbler where

import Circuit
import Circuit.Builder
import Circuit.Builder.Internals (runCircuitT)
import Circuit.Conversion
import Circuit.Utils
import Examples.Goldreich
import Examples.Simple

import Control.Monad
import Control.Monad.Trans (liftIO, lift)
import Data.Array
import Data.Array.IO
import Data.IORef
import Lens.Micro.Platform
import System.IO
import Text.Printf
import qualified Data.IntMap as IM

-- XXX: only fan-out one is secure at the moment
-- garbler that does not use permutation bits
garbler :: Int -> Int -> Int -> Circ2 -> IO (Acirc2, (Circ, Circ))
garbler securityParam paddingSize nindices c = runCircuitT $ do
    let ixLen = ceiling (fromIntegral (length (garbleableGates c)) ** (1 / fromIntegral nindices))

    -- seeds to the PRGs. the number of seeds is determined by the number of symbols in c.
    -- each seed corresponds to the inputs for each symbol, and is used to generate their wirelabels
    seeds <- replicateM (nsymbols c) (symbol securityParam)

    -- the main seed, used to generate all intermediate wirelabels
    s <- foldM1 (zipWithM circXor) seeds

    ix <- sigmaProd =<< replicateM nindices (sigma ixLen) -- the index of the gate to evaluate

    -- G1 is the PRG used to generate wirelabels
    (g1, g1Save) <- do
        let numWirelabels = 2*ninputs c + length (garbleableGates c) + 1
        g <- prgBuilder securityParam (securityParam * numWirelabels) 5 xorAnd
        let g' xs = safeChunksOf securityParam <$> g xs
        asCirc <- lift $ buildCircuitT (inputs securityParam >>= g >>= outputs)
        return (g', toCirc asCirc)

    -- G2 is the PRG used to encrypt the garbled tables
    (g2, g2Save) <- do
        g <- prgBuilder securityParam (2*(securityParam + paddingSize)) 5 xorAnd
        let g' i xs = (!! i) . safeChunksOf (securityParam+paddingSize) <$> g xs
        asCirc <- lift $ buildCircuitT (inputs securityParam >>= g >>= outputs)
        return (g', toCirc asCirc)

    let inputWires = []
    -- -- MIFE: generate each symbol's wires based only on its corresponding seed
    -- inputWires <- fmap concat $ forM (zip [SymId 0..] seeds) $ \(sym, seed) -> do
    --     (_:raw) <- g1 seed
    --     let refs = inputRefsForSymbol c sym
    --         ws = selectsPT raw (map getRef refs)
    --     return (zip refs ws)

    initialWires <- do
        -- construct the true wirelabels by xoring in delta
        (delta:falseWLs) <- g1 s

        fresh  <- liftIO $ newIORef falseWLs
        labels <- liftIO $ (newArray_ (0, Ref (nwires c-1)) :: IO (IOArray Ref ([Ref], [Ref])))

        forM_ (wires c) $ \(zref, g) -> case g of
            (Bool2Xor xref yref) | not (hasInputArg c g) -> do
                -- only use freeXOR for intermediate gates, this allows MIFE since freeXOR requires
                -- globally known delta, but we dont know it until we have all the seeds.
                x <- fst <$> liftIO (readArray labels xref)
                y <- fst <$> liftIO (readArray labels yref)
                z  <- zipWithM circXor x y
                z' <- zipWithM circXor z delta
                liftIO $ writeArray labels zref (z, z')
            _ -> do
                z  <- head <$> liftIO (readIORef fresh)
                z' <- zipWithM circXor z delta
                liftIO $ modifyIORef fresh tail
                liftIO $ writeArray labels zref (z, z')

        liftIO (freeze labels)

    -- update all wires with specially generated inputs
    let allWires = initialWires // inputWires :: Array Ref ([Ref], [Ref])

    mapM_ saveRef (allWires ^.. each.each.each)

    -- plaintext outputs for the output gates
    outWires <- do
        one  <- constant 1
        zero <- constant 0
        let zs = replicate (securityParam-1) zero
        return (zs++[zero], zs++[one])

    -- gate wires is a list of lists of the wires needed for the ith garbled gate, in the correct order
    gateWires <- lift $ randIO $ forM (garbleableGates c) $ \(zref,g) -> do
        let [xref,yref] = gateArgs g
            get stuff 0 = fst stuff
            get stuff 1 = snd stuff

        -- randomize the truth table for gate g
        tt <- randomize (permutations 2 [0,1])

        -- for each table entry, we need to know whether to encrypt the first or second wirelabel of z
        fmap concat $ forM tt $ \[i,j] -> do
            let x = get (allWires ! xref) i
                y = get (allWires ! yref) j
                z = if isOutputRef c zref
                    then get outWires (gateEval (const undefined) g [i,j])
                    else get (allWires ! zref) (gateEval (const undefined) g [i,j])
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

--------------------------------------------------------------------------------
-- helpers

isXor :: BoolGate2 -> Bool
isXor (Bool2Xor _ _) = True
isXor _              = False

hasInputArg :: Gate gate => Circuit gate -> gate -> Bool
hasInputArg c gate = any (not . gateIsGate) $ map (getGate c) (gateArgs gate)

garbleableGates :: Circ2 -> [(Ref, BoolGate2)]
garbleableGates c = filter ok (gates c)
  where
    ok (_,g) = not (isXor g) || hasInputArg c g