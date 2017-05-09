module Circuit.Format.Acirc
  ( Circuit.Format.Acirc.read
  , Circuit.Format.Acirc.write
  , readAcirc
  , writeAcirc
  , parseCirc
  , showCirc
  , showTest
  , genTestStr
  , addTestsToFile
  , showCircWithTests
  ) where

import Circuit
import Circuit.Parser
import Util

import Control.Monad
import qualified Control.Monad.State as S
import Text.Parsec hiding (spaces, parseTest)
import Text.Printf
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
    header = [ printf ":symlen %d" (circ_symlen c)
             , printf ":base %d" (circ_base c)
             ]

    inputs = mapM gateStr (circ_inputs c)
    consts = mapM gateStr (M.keys (circ_secret_refs c))
    gates  = mapM gateStr (nonInputGates c)

    output = do
        outs <- map show <$> mapM tr (circ_outputs c)
        secs <- map show <$> mapM tr (secretRefs c)
        return [ printf ":outputs %s" (unwords outs)
               , printf ":secrets %s" (unwords secs)
               ]

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
showTest (inp, out) = printf ":test %s %s" (showInts (reverse inp)) (showInts (reverse out))

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
    circParser = preamble >> lines >> end >> eof
    preamble = many $ (char ':' >> (parseTest <|> parseSymlen <|> parseBase <|> skipParam))
    lines    = many parseRefLine
    end      = parseOutputs >> optional parseSecrets

skipParam :: ParseCirc ()
skipParam = do
    skipMany (oneOf " \t" <|> alphaNum)
    endLine

parseTest :: ParseCirc ()
parseTest = do
    string "test"
    spaces
    inps <- many digit
    spaces
    outs <- many digit
    let inp = readInts inps
        res = readInts outs
    addTest (reverse inp, reverse res)
    endLine

parseBase :: ParseCirc ()
parseBase = do
    string "base"
    spaces
    n <- Prelude.read <$> many digit
    setBase n
    endLine

parseSymlen :: ParseCirc ()
parseSymlen = do
    string "symlen"
    spaces
    n <- Prelude.read <$> many digit
    setSymlen n
    endLine

parseOutputs :: ParseCirc ()
parseOutputs = do
    string ":outputs"
    spaces
    refs <- many (do ref <- parseRef; spaces; return ref)
    mapM_ markOutput refs
    endLine

parseSecrets :: ParseCirc ()
parseSecrets = do
    string ":secrets"
    spaces
    secs <- many (do ref <- parseRef; spaces; return ref)
    ys   <- (M.keys . circ_secret_refs) <$> getCirc
    forM_ ys $ \y -> do
        when (y `notElem` secs) $ do
            markPublicConst y
    endLine

parseRef :: ParseCirc Ref
parseRef = Ref <$> Prelude.read <$> many1 digit

parseRefLine :: ParseCirc ()
parseRefLine = do
    ref <- parseRef
    spaces
    choice [parseConst ref, parseInput ref, parseGate ref]
    endLine

parseInput :: Ref -> ParseCirc ()
parseInput ref = do
    string "input"
    spaces
    id <- Id <$> Prelude.read <$> many1 digit
    insertInput ref id

parseConst :: Ref -> ParseCirc ()
parseConst ref = do
    string "const"
    spaces
    val <- Prelude.read <$> many1 digit
    id  <- nextConstId
    insertSecret ref id
    insertSecretVal id val

parseGate :: Ref -> ParseCirc ()
parseGate ref = do
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
