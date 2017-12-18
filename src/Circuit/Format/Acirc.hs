module Circuit.Format.Acirc
  ( Circuit.Format.Acirc.read
  , Circuit.Format.Acirc.write
  , readWithTests
  , showWithTests
  ) where

import Circuit
import Circuit.Conversion
import Circuit.Parser
import Circuit.Utils
import qualified Circuit.Builder           as B
import qualified Circuit.Builder.Internals as B

import Control.Monad.Trans (lift)
import Lens.Micro.Platform
import Text.Parsec hiding (spaces, parseTest)
import Text.Printf
import qualified Data.Map as M
import qualified Control.Monad.State as S

read :: FilePath -> IO Acirc
read = fmap fst . readWithTests

readWithTests :: FilePath -> IO (Acirc, [TestCase])
readWithTests fp = parseCirc <$> readFile fp

write :: ToAcirc g => FilePath -> Circuit g -> IO ()
write fp c = writeFile (showCirc (toAcirc c)) fp

showWithTests :: ToAcirc g => Circuit g -> [TestCase] -> String
showWithTests c ts = let s = showCirc (toAcirc c)
                         t = unlines (map showTest ts)
                     in t ++ s

showCirc :: Acirc -> String
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
            (ArithInput  id) -> return $ printf "%d input %d" ref' (getId id)
            (ArithConst id) -> do
                let val = case c ^. circ_const_vals . at id  of
                                Nothing -> ""
                                Just y  -> show y
                return $ printf "%d const %s" ref' val
            (ArithAdd x y) -> pr ref' "ADD" x y
            (ArithSub x y) -> pr ref' "SUB" x y
            (ArithMul x y) -> pr ref' "MUL" x y

    pr :: Int -> String -> Ref -> Ref -> S.State (M.Map Ref Int, Int) String
    pr ref' gateTy x y = do
        x' <- tr x
        y' <- tr y
        return $ printf "%d %s %d %d" ref' gateTy x' y'

showTest :: TestCase -> String
showTest (inp, out) = printf ":test %s %s" (showInts (reverse inp)) (showInts (reverse out))

--------------------------------------------------------------------------------
-- parser

type AcircParser = ParseCirc ArithGate ()

parseCirc :: String -> (Acirc, [TestCase])
parseCirc s = runCircParser () parser s
  where
    parser   = preamble >> lines >> end >> eof
    preamble = many $ (char ':' >> (parseTest <|> parseSymlen <|> parseBase <|> skipParam))
    lines    = many parseRefLine
    end      = parseOutputs >> optional parseSecrets

skipParam :: AcircParser ()
skipParam = do
    skipMany (oneOf " \t" <|> alphaNum)
    endLine

parseTest :: AcircParser ()
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

parseBase :: AcircParser ()
parseBase = do
    string "base"
    spaces
    n <- Prelude.read <$> many digit
    lift (B.setBase n)
    endLine

parseSymlen :: AcircParser ()
parseSymlen = do
    string "symlen"
    spaces
    n <- Prelude.read <$> many digit
    lift $ B.setSymlen n
    endLine

parseOutputs :: AcircParser ()
parseOutputs = do
    string ":outputs"
    spaces
    refs <- many (do ref <- parseRef; spaces; return ref)
    lift $ mapM_ B.markOutput refs
    endLine

parseSecrets :: AcircParser ()
parseSecrets = do
    string ":secrets"
    spaces
    secs <- many (do { ref <- parseRef; spaces; return ref })
    lift $ mapM B.markSecret secs
    endLine

parseRef :: AcircParser Ref
parseRef = Ref <$> Prelude.read <$> many1 digit

parseRefLine :: AcircParser ()
parseRefLine = do
    ref <- parseRef
    spaces
    choice [parseConst ref, parseInput ref, parseGate ref]
    endLine

parseInput :: Ref -> AcircParser ()
parseInput ref = do
    string "input"
    spaces
    id <- Id <$> Prelude.read <$> many1 digit
    lift $ B.insertInput ref id

parseConst :: Ref -> AcircParser ()
parseConst ref = do
    string "const"
    spaces
    val <- Prelude.read <$> many1 digit
    id  <- lift B.nextConstId
    lift $ B.insertConst ref id
    lift $ B.insertConstVal id val

parseGate :: Ref -> AcircParser ()
parseGate ref = do
    opType <- oneOfStr ["ADD", "SUB", "MUL"]
    spaces
    x <- Ref . Prelude.read <$> many1 digit
    spaces
    y <- Ref . Prelude.read <$> many1 digit
    let gate = case opType of
            "ADD" -> ArithAdd x y
            "MUL" -> ArithMul x y
            "SUB" -> ArithSub x y
            g     -> error ("[parser] unkonwn gate type " ++ g)
    lift $ B.insertGate ref gate
