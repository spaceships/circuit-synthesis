module Circuit.Format.Acirc
  ( readAcirc
  , writeAcirc
  , parseCirc
  , showCirc
  , showTest
  , genTestStr
  , addTestsToFile
  ) where

import Circuit
import Circuit.Parser
import Util (forceM, readBits', showBits', safeInsert)

import Control.Monad
import qualified Control.Monad.State as S
import Text.Parsec hiding (spaces, parseTest)
import Text.Printf
import qualified Data.Set as S
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- printer

addTestsToFile :: FilePath -> IO ()
addTestsToFile fp = do
    s <- readFile fp
    let (c,_) = parseCirc s
    ts <- replicateM 10 (genTestStr c)
    forceM ts
    writeFile fp (unlines ts ++ s)

writeAcirc :: FilePath -> Circuit -> IO ()
writeAcirc fp c = do
    s <- showCircWithTests 10 c
    writeFile fp s

showCircWithTests :: Int -> Circuit -> IO String
showCircWithTests ntests c = do
    ts <- replicateM ntests (genTestStr c)
    return (unlines ts ++ showCirc c)

showCirc :: Circuit -> String
showCirc c = unlines (header ++ gateLines)
  where
    header = [printf ": nins %d" (ninputs c), printf ": depth %d" (depth c)]
    inputs = mapM (gateStr False) (circ_inputs c)
    consts = mapM (gateStr False) (circ_consts c)
    igates = mapM (gateStr False) (intermediateGates c)
    output = mapM (gateStr True)  (circ_outputs c)

    gateLines = concat $ S.evalState (sequence [inputs, consts, igates, output]) (M.empty, 0)

    -- we need to minimize the number of refs we use since we may not output
    -- them all
    tr :: Ref -> S.State (M.Map Ref Int, Int) Int
    tr ref = do
        (m,i) <- S.get
        case M.lookup ref m of
            Just j  -> return j
            Nothing -> do
                S.put (M.insert ref i m, i+1)
                return i

    gateStr :: Bool -> Ref -> S.State (M.Map Ref Int, Int) String
    gateStr isOutput ref = do
        ref' <- tr ref
        case M.lookup ref (circ_refmap c) of
            Nothing -> error (printf "[gateStr] unknown ref %s" (show ref))
            Just (OpInput id) -> return $ printf "%d input x%d" ref' (getId id)
            Just (OpConst id) -> do
                let secret = case M.lookup id (circ_secrets c) of
                                Nothing -> ""
                                Just y  -> show y
                return $ printf "%d input y%d %s" ref' (getId id) secret
            Just (OpAdd x y) -> pr ref' "ADD" x y isOutput
            Just (OpSub x y) -> pr ref' "SUB" x y isOutput
            Just (OpMul x y) -> pr ref' "MUL" x y isOutput

    pr :: Int -> String -> Ref -> Ref -> Bool -> S.State (M.Map Ref Int, Int) String
    pr ref' gateTy x y isOutput = do
        x' <- tr x
        y' <- tr y
        return $ printf "%d %s %s %d %d" ref' (if isOutput then "output" else "gate") gateTy x' y'

showTest :: TestCase -> String
showTest (inp, out) = printf "# TEST %s %s" (showBits' (reverse inp)) (showBits' (reverse out))

genTestStr :: Circuit -> IO String
genTestStr = fmap showTest . genTest

--------------------------------------------------------------------------------
-- parser

readAcirc :: FilePath -> IO (Circuit, [TestCase])
readAcirc fp = parseCirc <$> readFile fp

parseCirc :: String -> (Circuit, [TestCase])
parseCirc s = case runParser (circParser >> getState) emptySt "" s of
    Left err -> error (show err)
    Right st -> (st_circ st, reverse (st_tests st))
  where
    circParser = start >> rest >> eof
    start = many $ choice [parseParam, parseTest]
    rest  = many $ choice [try parseGate, try parseInput]

parseParam :: ParseCirc ()
parseParam = do
    char ':'
    skipMany (oneOf " \t" <|> alphaNum)
    endLine

parseTest :: ParseCirc ()
parseTest = do
    string "# TEST"
    spaces
    inps <- many (oneOf "01")
    spaces
    outs <- many (oneOf "01")
    let inp = readBits' inps
        res = readBits' outs
    addTest (reverse inp, reverse res)
    endLine

parseInput :: ParseCirc ()
parseInput = do
    gateRef <- Ref <$> read <$> many1 digit
    spaces
    string "input"
    spaces
    parseX gateRef <|> parseY gateRef
    endLine

parseX :: Ref -> ParseCirc ()
parseX ref = do
    char 'x'
    id <- Id <$> read <$> many1 digit
    insertInput ref id

parseY :: Ref -> ParseCirc ()
parseY ref = do
    char 'y'
    id <- Id <$> read <$> many1 digit
    spaces
    val <- read <$> many1 digit
    insertConst ref id
    insertSecret id val

parseGate :: ParseCirc ()
parseGate = do
    ref <- Ref <$> read <$> many1 digit
    spaces
    gateType <- oneOfStr ["gate", "output"]
    when (gateType == "output") $ markOutput ref
    spaces
    opType <- oneOfStr ["ADD", "SUB", "MUL"]
    spaces
    xref <- Ref <$> read <$> ((:) <$> option ' ' (char '-') <*> many1 digit)
    spaces
    yref <- Ref <$> read <$> many1 digit
    let op = case opType of
            "ADD" -> OpAdd xref yref
            "MUL" -> OpMul xref yref
            "SUB" -> OpSub xref yref
            g     -> error ("[parser] unkonwn gate type " ++ g)
    insertOp ref op
    endLine
