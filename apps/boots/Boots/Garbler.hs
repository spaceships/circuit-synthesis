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
import Control.Monad.Trans
import Data.Array
import Data.Array.IO
import Data.IORef
import Data.Maybe (fromMaybe)
import Lens.Micro.Platform
import System.Exit
import System.IO
import Text.Printf
import qualified Data.IntMap as IM
import qualified Data.Map as M

data GarblerParams = GarblerParams {
    securityParam :: Int,
    paddingSize   :: Int,
    numIndices    :: Int,
    gatesPerIndex :: Int
} deriving (Show, Read)

-- XXX: only fan-out one is secure at the moment
garbler :: GarblerParams -> Circ2 -> IO (Acirc2, (Circ, Circ, Bool))
garbler (GarblerParams {..}) c = runCircuitT $ do
    let numIterations = ceiling (fi (length (garbleableGates c)) / fi gatesPerIndex)
        ixLen = ceiling (fi numIterations ** (1 / fi numIndices))

        -- whether we dont need any indices
        naive = gatesPerIndex >= length (garbleableGates c)

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

    -- G2 is used to encrypt garbled rows, wrapper keeps track of which output to use
    (g2, g2Save) <- prgGen (securityParam+paddingSize) (2 * maximum (c^..circ_refcount.each))

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

    -- plaintext outputs for the output gates
    zero <- constant 0
    one  <- constant 1

    let pad      = replicate paddingSize zero
        outWires = (replicate securityParam zero, replicate securityParam one)

    count <- newRefCounter

    allGates <- forM (garbleableGates c) $ \(zref,g) -> do
        let [xref,yref] = gateArgs g
        xoffset <- count xref
        yoffset <- count yref
        gx0 <- drop (2*xoffset) <$> g2 (fst (allWires ! xref))
        gx1 <- drop (2*xoffset) <$> g2 (snd (allWires ! xref))
        gy0 <- drop (2*yoffset) <$> g2 (fst (allWires ! yref))
        gy1 <- drop (2*yoffset) <$> g2 (snd (allWires ! yref))

        -- randomize the truth table for gate g
        tt <- lift $ randIO $ randomize (permutations 2 [0,1])

        -- for each table entry, we need to know whether to encrypt the first or second wirelabel of z
        gate <- forM (zip tt (permutations 2 [0,1])) $ \([i,j], [i',j']) -> do
            let mx = get (gx0, gx1) i !! j'
                my = get (gy0, gy1) j !! i'
                z = if isOutputRef c zref
                    then get outWires (gateEval (const undefined) g [i,j])
                    else get (allWires ! zref) (gateEval (const undefined) g [i,j])
            foldM1 (zipWithM circXor) [mx, my, pad ++ z]
        return (concat gate)

    mapM_ saveRef (allGates ^.. each.each)

    ix <- sigmaProd =<< replicateM numIndices (sigma ixLen) -- the index to evaluate
    outputs =<< selectListSigma ix allGates

    return (g0Save, g2Save, naive) -- the PRGs for evaluation


genWiresGen :: GarblerParams -> Circ2 -> Circ -> Acirc2
genWiresGen (GarblerParams {..}) c g0 = buildCircuit $ do
    (seeds, inputs) <- fmap unzip $ forM [0..nsymbols c-1] $ \sym -> do
        seed  <- symbol securityParam
        input <- symbol (symlen c (SymId sym))
        return (seed, input)
    consts  <- mapM constant (constVals c)
    secrets <- mapM secret (secretVals c)

    combinedSeed <- foldM1 (zipWithM circXor) seeds
    (delta:rawInpWLs) <- safeChunksOf securityParam <$> subcircuit (toAcirc2 g0) combinedSeed

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

get :: (a,a) -> Int -> a
get stuff 0 = fst stuff
get stuff 1 = snd stuff
get _     _ = error "get only defined on 0 and 1!"

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

-- used to keep track of which output of G2 to use for encryption
newRefCounter :: MonadIO m => m (Ref -> m Int)
newRefCounter = do
    mRef <- liftIO (newIORef IM.empty)
    return $ \ref -> do
        count <- fromMaybe 0 . IM.lookup (getRef ref) <$> liftIO (readIORef mRef)
        liftIO $ modifyIORef mRef (IM.insertWith (+) (getRef ref) 1)
        return count
