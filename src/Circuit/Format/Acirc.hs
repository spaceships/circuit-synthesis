{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Circuit.Format.Acirc
  ( Circuit.Format.Acirc.read
  , Circuit.Format.Acirc.write
  , writeWithTests
  , readAcirc
  , showWithTests
  , showCirc
  , parseCirc
  ) where

import Circuit
import Circuit.Parser
import Circuit.Utils hiding ((%))
import qualified Circuit.Builder           as B
import qualified Circuit.Builder.Internals as B

import Control.Monad
import Control.Monad.Trans (lift)
import Lens.Micro.Platform
import Text.Parsec hiding (spaces, parseTest)
import TextShow
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.IntMap as IM

read :: FilePath -> IO Acirc
read = fmap fst . readAcirc

readAcirc :: FilePath -> IO (Acirc, [TestCase])
readAcirc fp = parseCirc <$> readFile fp

write :: FilePath -> Acirc -> IO ()
write fp c = T.writeFile fp (showCirc c)

writeWithTests :: FilePath -> Acirc -> IO ()
writeWithTests fp c = do
    ts <- replicateM 10 (genTest c)
    T.writeFile fp (showWithTests c ts)

showWithTests :: Acirc -> [TestCase] -> T.Text
showWithTests c ts = let s = showCirc c
                         t = T.unlines (map showTest ts)
                     in T.append t s

showCirc :: Acirc -> T.Text
showCirc !c = T.unlines (header ++ gateLines)
  where
    header = [ T.append ":symlen "  (showt (_circ_symlen c))
             , T.append ":base "    (showt (_circ_base c))
             , T.append ":ninputs " (showt (ninputs c))
             , T.append ":consts "  (T.unwords (map showt (IM.elems (_circ_const_vals c))))
             , T.append ":outputs " (T.unwords (map (showt.getRef) (outputRefs c)))
             , T.append ":secrets " (T.unwords (map (showt.getRef) (secretRefs c)))
             , ":start"
             ]

    inputs = map gateTxt (inputRefs c)
    consts = map gateTxt (constRefs c)
    gates  = map gateTxt (gateRefs c)

    gateLines = concat [inputs, consts, gates]

    showRef = showt . getRef

    gateTxt :: Ref -> T.Text
    gateTxt !ref =
        case c ^. circ_refmap . at (getRef ref) . non (error "[gateTxt] unknown ref") of
            (ArithInput id) -> T.concat [showRef ref, " input ", showt (getId id)]
            (ArithConst id) -> T.append (showRef ref) " const"
            (ArithAdd x y) -> pr ref "ADD" x y
            (ArithSub x y) -> pr ref "SUB" x y
            (ArithMul x y) -> pr ref "MUL" x y

    pr :: Ref -> T.Text -> Ref -> Ref -> T.Text
    pr !ref !gateTy !x !y =
        T.concat [ showRef ref, " ", gateTy, " ", showRef x, " ", showt (getRef y)
                 , " : ", showRefCount ref
                 ]

    showRefCount !ref = let count = c ^. circ_refcount . at (getRef ref) . non 0
                        in if count == (-1)
                              then "inf"
                              else showt count

showTest :: TestCase -> T.Text
showTest (!inp, !out) = T.concat [":test ", T.pack (showInts (reverse inp)), " "
                                 , T.pack (showInts (reverse out)) ]

--------------------------------------------------------------------------------
-- parser

type AcircParser = ParseCirc ArithGate Int

parseCirc :: String -> (Acirc, [TestCase])
parseCirc s = runCircParser 0 parser s
  where
    parser   = preamble >> lines >> eof
    preamble = many $ (char ':' >> (try parseTest <|> try parseSymlen <|> try parseBase <|>
                                    try parseOutputs <|> try parseSecrets <|> skipParam))
    lines    = many parseRefLine

nextRef :: AcircParser Ref
nextRef = do
    ref <- getSt
    modifySt (succ)
    return (Ref ref)

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
    string "outputs"
    spaces
    refs <- many (parseRef <* spaces)
    lift $ mapM_ B.markOutput refs
    endLine

parseSecrets :: AcircParser ()
parseSecrets = do
    string "secrets"
    spaces
    secs <- many (parseRef <* spaces)
    lift $ mapM B.markSecret secs
    endLine

parseConsts :: Ref -> AcircParser ()
parseConsts ref = do
    string "consts"
    cs <- many (spaces >> int)
    forM_ (zip [0..] cs) $ \(id, val) -> do
        lift $ B.insertConstVal (Id id) (fromIntegral val)

parseRef :: AcircParser Ref
parseRef = Ref <$> int

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
    id <- Id <$> int
    lift $ B.insertInput ref id

parseConst :: Ref -> AcircParser ()
parseConst ref = do
    string "const"
    spaces
    id <- lift B.nextConstId
    lift $ B.insertConst ref id

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
    optional $ spaces >> char ':' >> spaces >> int -- times used annotation
