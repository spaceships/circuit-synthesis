{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

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
import System.Exit
import System.IO
import Text.Printf
import qualified Data.IntMap as IM

data GarblerParams = GarblerParams {
    securityParam :: Int,
    paddingSize   :: Int,
    numIndices    :: Int,
    gatesPerIndex :: Int
} deriving (Show, Read)

-- XXX: only fan-out one is secure at the moment
garbler :: GarblerParams -> Circ2 -> IO (Acirc2, (Circ, Circ))
garbler (GarblerParams {..}) c = runCircuitT $ do
    let numIterations = ceiling (fi (length (garbleableGates c)) / fi gatesPerIndex)
        ixLen = ceiling (fi numIterations ** (1 / fi numIndices))

    -- seeds to the PRGs. the number of seeds is determined by the number of symbols in c.
    -- each seed corresponds to the inputs for each symbol, and is used to generate their wirelabels
    seeds <- replicateM (nsymbols c) (symbol securityParam)

    -- the main seed, used to generate all intermediate wirelabels
    combinedSeed <- foldM1 (zipWithM circXor) seeds

    -- prgGen generates a PRG that takes a securityParam size seed and outputs n chunks of size
    -- chunkSize, as well as a circuit version for saving
    let prgGen chunkSize n = do
            g <- prgBuilder securityParam (chunkSize * n) 5 xorAnd
            let g' xs = safeChunksOf chunkSize <$> g xs
            asCirc <- lift $ buildCircuitT (inputs securityParam >>= g >>= outputs)
            return (g', toCirc asCirc)

    -- G0 is used to generate input/const/secret wirelabels and delta
    (g0, g0Save) <- prgGen securityParam (1 + ninputs c + nconsts c + nsecrets c)

    -- G1 is used to generate intermediate wirelabels:
    -- G1 is not needed to evaluate the garbled circuit, so we dont save it
    (g1, _) <- prgGen securityParam (length (garbleableGates c))

    -- G2 is used to encrypt garbled rows
    (g2, g2Save) <- prgGen (securityParam+paddingSize) 2

    allWires <- do
        (delta:rawInpWLs) <- g0 combinedSeed

        let deltize f = do { t <- zipWithM circXor f delta; return (f,t) }
        freshInpWLs <- mapM deltize rawInpWLs

        let inputWLs  = listArray (0, InputId  (ninputs  c-1)) (take (ninputs c) freshInpWLs)
            constWLs  = listArray (0, ConstId  (nconsts  c-1)) (take (nconsts c) (drop (ninputs c) freshInpWLs))
            secretWLs = listArray (0, SecretId (nsecrets c-1)) (take (nsecrets c) (drop (ninputs c + nconsts c) freshInpWLs))

        fresh  <- liftIO . newIORef =<< g1 combinedSeed -- fresh intermediate WLs
        labels <- liftIO $ (newArray_ (0, Ref (nwires c-1)) :: IO (IOArray Ref ([Ref], [Ref])))

        let nextFresh = liftIO $ do
                whenM (null <$> readIORef fresh) $ do
                    putStrLn "[garbler] not enough outputs from G1!"
                    exitFailure
                z <- head <$> readIORef fresh
                modifyIORef fresh tail
                return z

        forM_ (wires c) $ \(zref, g) -> case g of
            (Bool2Base (Input  id)) -> do liftIO $ writeArray labels zref (inputWLs  ! id)
            (Bool2Base (Const  id)) -> do liftIO $ writeArray labels zref (constWLs  ! id)
            (Bool2Base (Secret id)) -> do liftIO $ writeArray labels zref (secretWLs ! id)

            (Bool2Xor xref yref) | not (isOutputRef c zref) -> do
                x <- fst <$> liftIO (readArray labels xref)
                y <- fst <$> liftIO (readArray labels yref)
                (f,t) <- deltize =<< zipWithM circXor x y
                liftIO $ writeArray labels zref (f,t)

            _ -> do
                (f,t) <- deltize =<< nextFresh
                liftIO $ writeArray labels zref (f,t)

        liftIO (freeze labels)

    mapM_ saveRef (allWires ^.. each.each.each)

    -- plaintext outputs for the output gates
    zero <- constant 0
    one  <- constant 1
    outWires <- do
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

    gateWLs <-
        if gatesPerIndex < length (garbleableGates c) then do
            -- relevant wires for this iteration
            let gatePad     = replicate (3*4*securityParam) zero
                wireBundles = map concat $ chunksOfPad gatesPerIndex gatePad gateWires
            ix <- sigmaProd =<< replicateM numIndices (sigma ixLen) -- the index to evaluate
            relevantSel <- selectListSigma ix wireBundles
            return $ safeChunksOf (3*4*securityParam) relevantSel
        else
            return gateWires

    let pad = replicate paddingSize zero

    forM_ gateWLs $ \gateWL -> do
        let ws = safeChunksOf 3 $ safeChunksOf securityParam gateWL
        forM_ (zip ws (permutations 2 [0,1])) $ \([x,y,z],[i,j]) -> do
            mx  <- (!!j) <$> g2 x
            my  <- (!!i) <$> g2 y
            row <- foldM1 (zipWithM circXor) [mx, my, pad ++ z]
            outputs row

    return (g0Save, g2Save) -- the PRGs for evaluation


genWiresGen :: GarblerParams -> Circ2 -> Circ -> Circ
genWiresGen (GarblerParams {..}) c g0 = buildCircuit $ do
    (seeds, inputs) <- fmap unzip $ forM [0..nsymbols c-1] $ \sym -> do
        seed  <- symbol securityParam
        input <- symbol (symlen c (SymId sym))
        return (seed, input)
    consts  <- mapM constant (constVals c)
    secrets <- mapM secret (secretVals c)

    combinedSeed <- foldM1 (zipWithM circXor) seeds
    (delta:rawInpWLs) <- safeChunksOf securityParam <$> subcircuit g0 combinedSeed

    let deltize f = do { t <- zipWithM circXor f delta; return (t,f) }
    freshInpWLs <- mapM deltize rawInpWLs

    let inputWLs  = take (ninputs c) freshInpWLs
        constWLs  = take (nconsts c) (drop (ninputs c) freshInpWLs)
        secretWLs = take (nsecrets c) (drop (ninputs c + nconsts c) freshInpWLs)

    outputs =<< concat <$> zipWithM bitSelectPairLists (concat inputs) inputWLs
    outputs =<< concat <$> zipWithM bitSelectPairLists consts  constWLs
    outputs =<< concat <$> zipWithM bitSelectPairLists secrets secretWLs


--------------------------------------------------------------------------------
-- helpers


isXor :: BoolGate2 -> Bool
isXor (Bool2Xor _ _) = True
isXor _              = False

hasInputArg :: Circ2 -> BoolGate2 -> Bool
hasInputArg c gate = any isInput $ map (getGate c) (gateArgs gate)
  where
    isInput (Bool2Base (Input _)) = True
    isInput _ = False

garbleableGates :: Circ2 -> [(Ref, BoolGate2)]
garbleableGates c = filter garbleMe (gates c)
  where
    garbleMe (ref,g) = not (isXor g) || isOutputRef c ref

fi = fromIntegral
