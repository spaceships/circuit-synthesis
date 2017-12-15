module Circuit.Format.Acirc
  ( Circuit.Format.Acirc.read
  , Circuit.Format.Acirc.write
  , readAcirc
  , writeAcirc
  , parseCirc
  , showCirc
  , showSortedCirc
  , showTest
  , genTestStr
  , addTestsToFile
  , showCircWithTests
  ) where

import Circuit
import Circuit.Parser
import Circuit.Utils
import qualified Circuit.Builder.Internals as B

import Control.Monad
import Control.Monad.Trans (lift)
import Lens.Micro.Platform
import Text.Parsec hiding (spaces, parseTest)
import Text.Printf
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Control.Monad.State as S

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

showSortedCirc :: Circuit -> String
showSortedCirc c = unlines (header ++ gateLines ++ output)
  where
    header = [ printf ":symlen %d" (_circ_symlen c)
             , printf ":base %d" (_circ_base c)
             ]

    inputs = map gateStr (_circ_inputs c)
    consts = map gateStr (M.keys (_circ_consts c))
    gates  = map gateStr (sortedNonInputGates c)
    gateLines = concat [inputs, consts, gates]
    output = let outs = map show (_circ_outputs c)
                 secs = map show (secretRefs c)
             in [ printf ":outputs %s" (unwords outs)
                , printf ":secrets %s" (unwords secs)
                ]

    gateStr :: Ref -> String
    gateStr ref = do
        case c ^. circ_refmap . at (getRef ref) . non (error "[gateStr] unknown ref") of
            (OpInput id) -> printf "%d input %d" (getRef ref) (getId id)
            (OpConst id) ->
                let val = case c ^. circ_const_vals . at id of
                                Nothing -> ""
                                Just y  -> show y
                in printf "%d const %s" (getRef ref) val
            (OpAdd x y) -> printf "%d ADD %d %d" (getRef ref) (getRef x) (getRef y)
            (OpSub x y) -> printf "%d SUB %d %d" (getRef ref) (getRef x) (getRef y)
            (OpMul x y) -> printf "%d MUL %d %d" (getRef ref) (getRef x) (getRef y)

showCirc :: Circuit -> String
showCirc c = unlines (header ++ gateLines)
  where
    header = [ printf ":symlen %d" (_circ_symlen c)
             , printf ":base %d" (_circ_base c)
             ]

    inputs = mapM gateStr (_circ_inputs c)
    consts = mapM gateStr (M.keys (_circ_consts c))
    gates  = mapM gateStr (gateRefs c)

    output = do
        outs <- map show <$> mapM tr (_circ_outputs c)
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
        case c ^. circ_refmap . at (getRef ref) . non (error "[gateStr] unknown ref") of
            (OpInput  id) -> return $ printf "%d input %d" ref' (getId id)
            (OpConst id) -> do
                let val = case c ^. circ_const_vals . at id  of
                                Nothing -> ""
                                Just y  -> show y
                return $ printf "%d const %s" ref' val
            (OpAdd x y) -> pr ref' "ADD" x y
            (OpSub x y) -> pr ref' "SUB" x y
            (OpMul x y) -> pr ref' "MUL" x y

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
parseCirc s = runCircParser circParser s
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
    lift (B.setBase n)
    endLine

parseSymlen :: ParseCirc ()
parseSymlen = do
    string "symlen"
    spaces
    n <- Prelude.read <$> many digit
    lift $ B.setSymlen n
    endLine

parseOutputs :: ParseCirc ()
parseOutputs = do
    string ":outputs"
    spaces
    refs <- many (do ref <- parseRef; spaces; return ref)
    lift $ mapM_ B.markOutput refs
    endLine

parseSecrets :: ParseCirc ()
parseSecrets = do
    string ":secrets"
    spaces
    secs <- many (do { ref <- parseRef; spaces; return ref })
    lift $ mapM B.markSecret secs
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
    lift $ B.insertInput ref id

parseConst :: Ref -> ParseCirc ()
parseConst ref = do
    string "const"
    spaces
    val <- Prelude.read <$> many1 digit
    id  <- lift B.nextConstId
    lift $ B.insertConst ref id
    lift $ B.insertConstVal id val

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
    lift $ B.insertOp ref op
