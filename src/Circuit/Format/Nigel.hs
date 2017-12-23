{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Circuit.Format.Nigel
    ( Circuit.Format.Nigel.read
    , readNigel
    , write
    , showCirc
    )
where

import Circuit
import Circuit.Parser
import Circuit.Conversion
import qualified Circuit.Builder as B

import Control.Monad
import Control.Monad.Trans (lift)
import Formatting ((%))
import Lens.Micro.Platform
import Prelude hiding (lookup)
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.IntMap as IM
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T
import qualified Control.Monad.State.Strict as S
import qualified Control.Monad.Writer.Strict as W
import qualified Formatting as F

read :: Gate g => FilePath -> IO (Circuit g)
read file = fst <$> readNigel file

readNigel :: Gate g => FilePath -> IO (Circuit g, [TestCase])
readNigel file = parseNigel <$> readFile file

write :: ToCirc g => FilePath -> Circuit g -> IO ()
write fp c = T.writeFile fp (showCirc (toCirc c))

--------------------------------------------------------------------------------
-- write

showCirc :: Circ -> T.Text
showCirc c = W.execWriter $ flip S.runStateT initial $ do
    W.tell (T.append header1 (T.pack "\n"))
    W.tell (T.append header2 (T.pack "\n\n"))
    foldCircM eval c
  where
    header1 = T.unwords $ map (T.pack . show) [ngates c - ninputs c, ngates c]
    header2 = T.unwords $ map (T.pack . show) [ninputs c, nconsts c, noutputs c]

    inputMappings  = M.fromList (zip (c^.circ_inputs ++ map Ref (IM.keys (c^.circ_consts))) [0..])
    outputMappings = M.fromList (zip (c^.circ_outputs) [ngates c - noutputs c..])
    initial = (M.union inputMappings outputMappings, ninputs c + nconsts c)

    eval :: BoolGate -> Ref -> a -> S.StateT (M.Map Ref Int, Int) (W.Writer T.Text) ()
    eval gate ref _ = do
        w <- getWire ref
        case gate of
            (BoolXor x y) -> do
                a <- use (_1 . at x . non (error "oops"))
                b <- use (_1 . at y . non (error "oops"))
                W.tell $ F.format ("2 1 " % F.int % " " % F.int % " " % F.int % " XOR\n") a b w
                _1 . at ref ?= w
            (BoolAnd x y) -> do
                a <- use (_1 . at x . non (error "oops"))
                b <- use (_1 . at y . non (error "oops"))
                W.tell $ F.format ("2 1 " % F.int % " " % F.int % " " % F.int % " AND\n") a b w
                _1 . at ref ?= w
            (BoolNot x) -> do
                a <- use (_1 . at x . non (error "oops"))
                W.tell $ F.format ("1 1 " % F.int % " " % F.int % " INV\n") a w
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

parseNigel :: Gate g => String -> (Circuit g, [TestCase])
parseNigel s = runCircParser IM.empty parseCircuit s

parseCircuit :: Gate g => ParseNigel g ()
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

parseLine :: Gate g => ParseNigel g ()
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
