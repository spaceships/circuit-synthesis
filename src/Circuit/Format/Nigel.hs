{-# LANGUAGE LambdaCase #-}

module Circuit.Format.Nigel
    ( Circuit.Format.Nigel.read
    , readNigel
    , write
    )
where

import Circuit
import Circuit.Parser
import Circuit.Conversion
import qualified Circuit.Builder as B

import Control.Monad
import Control.Monad.Trans (lift)
import Lens.Micro.Platform
import Text.Parsec hiding (spaces, parseTest)
import Text.Printf
import Prelude hiding (lookup)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.Writer.Strict as W

read :: GateEval g => FilePath -> IO (Circuit g)
read file = fst <$> readNigel file

readNigel :: GateEval g => FilePath -> IO (Circuit g, [TestCase])
readNigel file = parseNigel <$> readFile file

write :: ToCirc g => FilePath -> Circuit g -> IO ()
write fp c = writeFile fp (showCirc (toCirc c))

--------------------------------------------------------------------------------
-- write

showCirc :: Circ -> String
showCirc c = W.execWriter $ flip S.runStateT initial $ do
    W.tell (header1 ++ "\n")
    W.tell (header2 ++ "\n\n")
    foldCircM eval c
  where
    header1 = unwords $ map show [ngates c - ninputs c, ngates c]
    header2 = unwords $ map show [ninputs c, nconsts c, noutputs c]

    inputMappings  = M.fromList (zip (c^.circ_inputs ++ c^.circ_consts.to M.keys) [0..])
    outputMappings = M.fromList (zip (c^.circ_outputs) [ngates c - noutputs c..])
    initial = (M.union inputMappings outputMappings, ninputs c + nconsts c)

    eval :: BoolGate -> Ref -> a -> S.StateT (M.Map Ref Int, Int) (W.Writer String) ()
    eval gate ref _ = do
        w <- getWire ref
        case gate of
            (BoolXor x y) -> do
                a <- use (_1 . at x . non (error "oops"))
                b <- use (_1 . at y . non (error "oops"))
                W.tell $ printf "2 1 %d %d %d XOR\n" a b w
                _1 . at ref ?= w
            (BoolAnd x y) -> do
                a <- use (_1 . at x . non (error "oops"))
                b <- use (_1 . at y . non (error "oops"))
                W.tell $ printf "2 1 %d %d %d AND\n" a b w
                _1 . at ref ?= w
            (BoolNot x) -> do
                a <- use (_1 . at x . non (error "oops"))
                W.tell $ printf "1 1 %d %d INV\n" a w
                _1 . at ref ?= w
            _ -> return ()

    getWire ref = use (_1 . at ref) >>= \case
        Just wire -> return wire
        Nothing   -> do
            wire <- nextWire
            _1 . at ref ?= wire
            return wire

    nextWire = do w <- use _2; _2 += 1; return w

--------------------------------------------------------------------------------
-- parse

type ParseNigel g = ParseCirc g (IM.IntMap Ref)

parseNigel :: GateEval g => String -> (Circuit g, [TestCase])
parseNigel s = runCircParser IM.empty parseCircuit s

parseCircuit :: GateEval g => ParseNigel g ()
parseCircuit = do
    _ <- int
    spaces
    nwires <- int
    endLine
    ninputs <- int
    spaces
    nsecrets <- int
    spaces
    noutputs <- int
    endLine
    spaces
    endLine
    xs <- lift (B.inputs ninputs)
    ys <- lift (B.secrets (replicate nsecrets 0))
    refMerge (IM.fromList (zip [0..] xs))       -- update refs for inputs
    refMerge (IM.fromList (zip [ninputs..] ys)) -- update refs for secrets

    void $ many parseLine -- parse the gates

    let nigelOutputs = reverse (take noutputs [nwires-1, nwires-2..])
    acircOutputs <- mapM refLookup nigelOutputs
    lift $ B.outputs acircOutputs

parseLine :: GateEval g => ParseNigel g ()
parseLine = do
    nargs <- int
    spaces
    nouts <- int
    when (nouts /= 1) (error "[parseLine] nouts /= 1!!!")
    spaces
    args <- replicateM nargs $ do
        arg <- int
        spaces
        return arg
    out <- int
    spaces
    ty <- choice $ map string ["AND", "XOR", "INV"]
    z <- case ty of
        "AND" -> do
            [x,y] <- mapM refLookup args
            lift $ B.circMul x y
        "XOR" -> do
            [x,y] <- mapM refLookup args
            lift $ B.circXor x y
        "INV" -> do
            [x] <- mapM refLookup args
            lift $ B.circNot x
        g -> error $ "[parseLine] unknown gate " ++ g
    refUpdate out z
    endLine
