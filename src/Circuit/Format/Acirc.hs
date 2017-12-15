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

import Control.Monad
import Control.Monad.Trans (lift)
import Lens.Micro.Platform
import Text.Parsec hiding (spaces, parseTest)
import Text.Printf
import qualified Data.Map as M
import qualified Control.Monad.State as S

read :: GateEval g => FilePath -> IO (Circuit g)
read = fmap fst . readWithTests

readWithTests :: GateEval g => FilePath -> IO (Circuit g, [TestCase])
readWithTests fp = parseCirc <$> readFile fp

write :: ToAcirc g => FilePath -> Circuit g -> IO ()
write fp c = writeFile (showCirc (toAcirc c)) fp

showWithTests :: ToAcirc g => Circuit g -> [TestCase] -> String
showWithTests c ts = let s = showCirc (toAcirc c)
                         t = unlines (map showTest ts)
                     in t ++ s

showCirc :: Circuit ArithGate -> String
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

parseCirc :: GateEval g => String -> (Circuit g, [TestCase])
parseCirc s = runCircParser circParser s
  where
    circParser = preamble >> lines >> end >> eof
    preamble = many $ (char ':' >> (parseTest <|> parseSymlen <|> parseBase <|> skipParam))
    lines    = many parseRefLine
    end      = parseOutputs >> optional parseSecrets

skipParam :: ParseCirc g ()
skipParam = do
    skipMany (oneOf " \t" <|> alphaNum)
    endLine

parseTest :: ParseCirc g ()
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

parseBase :: ParseCirc g ()
parseBase = do
    string "base"
    spaces
    n <- Prelude.read <$> many digit
    lift (B.setBase n)
    endLine

parseSymlen :: ParseCirc g ()
parseSymlen = do
    string "symlen"
    spaces
    n <- Prelude.read <$> many digit
    lift $ B.setSymlen n
    endLine

parseOutputs :: ParseCirc g ()
parseOutputs = do
    string ":outputs"
    spaces
    refs <- many (do ref <- parseRef; spaces; return ref)
    lift $ mapM_ B.markOutput refs
    endLine

parseSecrets :: ParseCirc g ()
parseSecrets = do
    string ":secrets"
    spaces
    secs <- many (do { ref <- parseRef; spaces; return ref })
    lift $ mapM B.markSecret secs
    endLine

parseRef :: ParseCirc g Ref
parseRef = Ref <$> Prelude.read <$> many1 digit

parseRefLine :: GateEval g => ParseCirc g ()
parseRefLine = do
    existingRef <- parseRef
    spaces
    newRef <- choice [parseConst, parseInput, parseGate]
    refUpdate (getRef existingRef) newRef
    endLine

parseInput :: GateEval g => ParseCirc g Ref
parseInput = do
    ref <- lift $ B.nextRef
    string "input"
    spaces
    id <- Id <$> Prelude.read <$> many1 digit
    lift $ B.insertInput ref id
    return ref

parseConst :: GateEval g => ParseCirc g Ref
parseConst = do
    ref <- lift $ B.nextRef
    string "const"
    spaces
    val <- Prelude.read <$> many1 digit
    id  <- lift B.nextConstId
    lift $ B.insertConst ref id
    lift $ B.insertConstVal id val
    return ref

parseGate :: GateEval g => ParseCirc g Ref
parseGate = do
    opType <- oneOfStr ["ADD", "SUB", "MUL"]
    spaces
    x <- refLookup =<< Prelude.read <$> many1 digit
    spaces
    y <- refLookup =<< Prelude.read <$> many1 digit
    case opType of
            "ADD" -> lift $ B.circAdd x y
            "MUL" -> lift $ B.circMul x y
            "SUB" -> lift $ B.circSub x y
            g     -> error ("[parser] unkonwn gate type " ++ g)
