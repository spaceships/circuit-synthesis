module Circuit.Format.Nigel where

import Circuit hiding (ngates, ninputs, nsecrets, noutputs)
import Circuit.Parser
import qualified Circuit.Builder as B

import Control.Monad
import Control.Monad.Trans (lift)
import Text.Parsec hiding (spaces, parseTest)
import Prelude hiding (lookup)
import qualified Data.IntMap as IM

read :: GateEval g => FilePath -> IO (Circuit g)
read file = fst <$> readNigel file

readNigel :: GateEval g => FilePath -> IO (Circuit g, [TestCase])
readNigel file = runCircParser IM.empty parseCircuit <$> readFile file

type ParseNigel g = ParseCirc g (IM.IntMap Ref)

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
