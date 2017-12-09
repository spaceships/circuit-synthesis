module Circuit.Format.Nigel where

import Circuit
import Circuit.Parser
import Circuit.Utils
import qualified Circuit.Builder as B

import Control.Monad
import Control.Monad.Trans (lift)
import Text.Parsec hiding (spaces, parseTest)
import Text.Printf
import Prelude hiding (lookup)
import qualified Data.Map as M
import qualified Control.Monad.State as S

read :: FilePath -> IO Circuit
read file = fst <$> readNigel file

readNigel :: FilePath -> IO (Circuit, [TestCase])
readNigel file = parseNigel <$> readFile file

parseNigel :: String -> (Circuit, [TestCase])
parseNigel s = S.evalState (runCircParserT parseCircuit s) M.empty

-- while parsing, we need to keep track of how Nigel refs correspond to our refs
type ParseNigel = ParseCircT (S.State (M.Map Int Ref))

update :: Int -> Ref -> ParseNigel ()
update nigelRef acircRef = (lift.lift) $ S.modify (M.insert nigelRef acircRef)

merge :: M.Map Int Ref -> ParseNigel ()
merge map = (lift.lift) $ S.modify (M.union map)

lookup :: Int -> ParseNigel Ref
lookup nigelRef = (lift.lift) $ S.gets (M.! nigelRef)

parseCircuit :: ParseNigel ()
parseCircuit = do
    ngates <- int
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
    merge (M.fromList (zip [0..] xs))       -- update refs for inputs
    merge (M.fromList (zip [ninputs..] ys)) -- update refs for secrets

    many parseLine -- parse the gates

    let nigelOutputs = reverse (take noutputs [nwires-1, nwires-2..])
    acircOutputs <- mapM lookup nigelOutputs
    lift $ B.outputs acircOutputs

parseLine :: ParseNigel ()
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
            [x,y] <- mapM lookup args
            lift $ B.circMul x y
        "XOR" -> do
            [x,y] <- mapM lookup args
            lift $ B.circXor x y
        "INV" -> do
            [x] <- mapM lookup args
            lift $ B.circNot x
    update out z
    endLine
