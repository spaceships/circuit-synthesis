{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Circuit.Format.Acirc
  ( Circuit.Format.Acirc.read
  , Circuit.Format.Acirc.write
  , readWithTests
  , showWithTests
  , showCirc
  , parseCirc
  ) where

import Circuit
import Circuit.Parser
import Circuit.Utils hiding ((%))
import qualified Circuit.Builder           as B
import qualified Circuit.Builder.Internals as B

import Control.Monad.Trans (lift)
import Formatting ((%))
import Lens.Micro.Platform
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import qualified Formatting as F

read :: FilePath -> IO Acirc
read = fmap fst . readWithTests

readWithTests :: FilePath -> IO (Acirc, [TestCase])
readWithTests fp = parseCirc <$> readFile fp

write :: FilePath -> Acirc -> IO ()
write fp c = T.writeFile fp (showCirc c)

showWithTests :: Acirc -> [TestCase] -> T.Text
showWithTests c ts = let s = showCirc c
                         t = T.unlines (map showTest ts)
                     in T.append t s

showCirc :: Acirc -> T.Text
showCirc !c = T.unlines (header ++ gateLines)
  where
    header = [ F.sformat (":symlen " % F.int) (_circ_symlen c)
             , F.sformat (":base "   % F.int) (_circ_base c)
             ]

    inputs = map gateStr (_circ_inputs c)
    consts = map gateStr (M.keys (_circ_consts c))
    gates  = map gateStr (nonInputGateRefs c)

    output = [ F.sformat (":outputs " % F.string) (unwords (map show (c^.circ_outputs)))
             , F.sformat (":secrets " % F.string) (unwords (map show (secretRefs c)))
             ]

    gateLines = concat [inputs, consts, gates, output]

    gateStr :: Ref -> T.Text
    gateStr !ref = do
        case c ^. circ_refmap . at (getRef ref) . non (error "[gateStr] unknown ref") of
            (ArithInput id) -> F.sformat (F.int % " input " % F.int) (getRef ref) (getId id)
            (ArithConst id) ->
                let val = case c ^. circ_const_vals . at id  of
                                Nothing -> ""
                                Just y  -> show y
                in F.sformat (F.int % " const " % F.string) (getRef ref) val
            (ArithAdd x y) -> pr ref "ADD" x y
            (ArithSub x y) -> pr ref "SUB" x y
            (ArithMul x y) -> pr ref "MUL" x y

    pr :: Ref -> String -> Ref -> Ref -> T.Text
    pr !ref !gateTy !x !y =
        F.sformat (F.int % " " % F.string % " " % F.int % " " % F.int)
                  (getRef ref) gateTy (getRef x) (getRef y)

showTest :: TestCase -> T.Text
showTest (!inp, !out) = F.sformat (":test " % F.string % " " % F.string)
                                  (showInts (reverse inp)) (showInts (reverse out))

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
