module Circuit.Format.Acirc
  ( Circuit.Format.Acirc.read
  , Circuit.Format.Acirc.write
  , readAcirc
  , writeAcirc
  , writeAcircR
  , parseCirc
  , showCirc
  , showTest
  , genTestStr
  , addTestsToFile
  , showCircWithTests
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

read :: FilePath -> IO Circuit
read = fmap fst . readAcirc

write :: FilePath -> Circuit -> IO ()
write = writeAcirc

addTestsToFile :: FilePath -> IO ()
addTestsToFile fp = do
    s <- readFile fp
    let (c,_) = parseCirc s
    ts <- replicateM 10 (genTestStr 1 c)
    forceM ts
    writeFile fp (unlines ts ++ s)

writeAcirc :: FilePath -> Circuit -> IO ()
writeAcirc fp c = do
    s <- showCircWithTests 10 c
    writeFile fp s

writeAcircR :: FilePath -> Int -> Circuit -> IO ()
writeAcircR fp symlen c = do
    s <- showCircWithTestsR symlen 10 c
    writeFile fp s

showCircWithTests :: Int -> Circuit -> IO String
showCircWithTests ntests c = showCircWithTestsR 1 ntests c

showCircWithTestsR :: Int -> Int -> Circuit -> IO String
showCircWithTestsR symlen ntests c = do
    ts <- replicateM ntests (genTestStr symlen c)
    return (unlines ts ++ showCirc symlen c)

showCirc :: Int -> Circuit -> String
showCirc symlen c = unlines (header ++ gateLines)
  where
    header = [printf ":nins %d" (ninputs c), printf ":depth %d" (depth c)] ++
             if symlen /= 1 then [printf ":symlen %d" symlen] else []

    inputs = mapM gateStr (circ_inputs c)
    consts = mapM gateStr (M.keys (circ_secret_refs c))
    gates  = mapM gateStr (nonInputGates c)

    output = do
        outs <- map show <$> mapM tr (circ_outputs c)
        return [printf ":outputs %s" (unwords outs)]

    gateLines = concat $ S.evalState (sequence [inputs, consts, gates, output]) (M.empty, 0)

    -- we need to minimize the number of refs we use since we may not output them all
    tr :: Ref -> S.State (M.Map Ref Int, Int) Int
    tr ref = do
        (m,i) <- S.get
        case M.lookup ref m of
            Just j  -> return j
            Nothing -> do
                S.put (M.insert ref i m, i+1)
                return i

    gateStr :: Ref -> S.State (M.Map Ref Int, Int) String
    gateStr ref = do
        ref' <- tr ref
        case M.lookup ref (circ_refmap c) of
            Nothing -> error (printf "[gateStr] unknown ref %s" (show ref))
            Just (OpInput  id) -> return $ printf "%d input %d" ref' (getId id)
            Just (OpSecret id) -> do
                let secret = case M.lookup id (circ_secrets c) of
                                Nothing -> ""
                                Just y  -> show y
                return $ printf "%d const %s" ref' secret
            Just (OpAdd x y) -> pr ref' "ADD" x y
            Just (OpSub x y) -> pr ref' "SUB" x y
            Just (OpMul x y) -> pr ref' "MUL" x y

    pr :: Int -> String -> Ref -> Ref -> S.State (M.Map Ref Int, Int) String
    pr ref' gateTy x y = do
        x' <- tr x
        y' <- tr y
        return $ printf "%d %s %d %d" ref' gateTy x' y'

showTest :: TestCase -> String
showTest (inp, out) = printf ":test %s %s" (showBits' (reverse inp)) (showBits' (reverse out))

genTestStr :: Int -> Circuit -> IO String
genTestStr symlen c = fmap showTest (genTest symlen c)

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
    rest  = many $ choice [try parseGate, try parseInput, try parseConst, try parseOutputs]

parseParam :: ParseCirc ()
parseParam = do
    char ':'
    skipMany (oneOf " \t" <|> alphaNum)
    endLine

parseRef :: ParseCirc Ref
parseRef = Ref <$> Prelude.read <$> many1 digit

parseOutputs :: ParseCirc ()
parseOutputs = do
    string ":outputs"
    spaces
    refs <- many (do ref <- parseRef; spaces; return ref)
    mapM_ markOutput refs
    endLine

parseTest :: ParseCirc ()
parseTest = do
    string ":test"
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
    ref <- parseRef
    spaces
    string "input"
    spaces
    id <- Id <$> Prelude.read <$> many1 digit
    insertInput ref id
    endLine

parseConst :: ParseCirc ()
parseConst = do
    ref <- parseRef
    spaces
    string "const"
    spaces
    val <- Prelude.read <$> many1 digit
    id  <- nextConstId
    insertSecret ref id
    insertSecretVal id val
    endLine

parseGate :: ParseCirc ()
parseGate = do
    ref <- parseRef
    spaces
    -- gateType <- oneOfStr ["gate", "output"]
    -- when (gateType == "output") $ markOutput ref
    opType <- oneOfStr ["ADD", "SUB", "MUL"]
    spaces
    -- xref <- Ref <$> read <$> ((:) <$> option ' ' (char '-') <*> many1 digit)
    xref <- Ref <$> Prelude.read <$> many1 digit
    spaces
    yref <- Ref <$> Prelude.read <$> many1 digit
    let op = case opType of
            "ADD" -> OpAdd xref yref
            "MUL" -> OpMul xref yref
            "SUB" -> OpSub xref yref
            g     -> error ("[parser] unkonwn gate type " ++ g)
    insertOp ref op
    endLine
