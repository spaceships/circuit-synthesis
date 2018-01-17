{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Circuit.Format.Acirc
  ( Circuit.Format.Acirc.read
  , Circuit.Format.Acirc.write
  , Circuit.Format.Acirc.show
  , writeWithTests
  , readWithTests
  , showWithTests
  , parse
  ) where

import Circuit
import Circuit.Parser
import Circuit.Utils hiding ((%))
import qualified Circuit.Builder           as B
import qualified Circuit.Builder.Internals as B
import Prelude hiding (show)

import Control.Monad
import Control.Monad.Trans (lift)
import Data.Maybe (mapMaybe)
import Lens.Micro.Platform
import Text.Parsec hiding (spaces, parseTest, parse)
import TextShow
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.IntMap as IM

import Debug.Trace

read :: FilePath -> IO Acirc
read = fmap fst . readWithTests

readWithTests :: FilePath -> IO (Acirc, [TestCase])
readWithTests fp = parse <$> readFile fp

write :: FilePath -> Acirc -> IO ()
write fp c = T.writeFile fp (show c)

writeWithTests :: FilePath -> Acirc -> IO ()
writeWithTests fp c = do
    ts <- replicateM 10 (genTest c)
    T.writeFile fp (showWithTests c ts)

show :: Acirc -> T.Text
show c = showWithTests c []

showWithTests :: Acirc -> [TestCase] -> T.Text
showWithTests !c !ts = T.unlines (header ++ gateLines)
  where
    header = [ T.append ":ninputs " (showt (ninputs c))
             , T.append ":nrefs "   (showt (c ^. circ_maxref + 1))
             , T.append ":consts "  (T.unwords (map showt (IM.elems (_circ_const_vals c))))
             , T.append ":outputs " (T.unwords (map (showt.getRef) (outputRefs c)))
             , T.append ":secrets " (T.unwords (map (showt.getRef) (secretRefs c)))
             , T.append ":symlen "  (T.unwords (map showt (c^..circ_symlen.each)))
             , T.append ":base "    (showt (c^.circ_base))
             ] ++ map showTest ts
               ++ [ ":start" ]

    inputs = mapMaybe gateTxt (inputRefs c)
    consts = mapMaybe gateTxt (constRefs c)

    gates  = mapMaybe gateTxt (gateRefs c)

    gateLines = concat [inputs, consts, gates]

    showRef = showt . getRef

    gateTxt :: Ref -> Maybe T.Text
    gateTxt !ref =
        case c ^. circ_refcount . at (getRef ref) of
            Nothing -> Nothing
            Just ct -> Just $ case c ^. circ_refmap . at (getRef ref) . non (error "[gateTxt] unknown ref") of
                (ArithInput id) -> T.concat [showRef ref, " input ", showt (getId id)
                                            ," : ", showCount ct]
                (ArithConst id) -> T.concat [showRef ref, " const : ", showCount ct]
                (ArithAdd x y) -> pr ref "ADD" x y ct
                (ArithSub x y) -> pr ref "SUB" x y ct
                (ArithMul x y) -> pr ref "MUL" x y ct

    pr :: Ref -> T.Text -> Ref -> Ref -> Int -> T.Text
    pr !ref !gateTy !x !y ct =
        T.concat [ showRef ref, " ", gateTy, " ", showRef x, " ", showt (getRef y)
                 , " : ", showCount ct
                 ]

    showCount ct = if ct == -1 then "inf" else showt ct

showTest :: TestCase -> T.Text
showTest (!inp, !out) = T.concat [":test ", T.pack (showInts inp), " ", T.pack (showInts out) ]

--------------------------------------------------------------------------------
-- parser

type AcircParser = ParseCirc ArithGate Int

parse :: String -> (Acirc, [TestCase])
parse s = runCircParser 0 parser s
  where
    parser   = preamble >> lines >> eof
    preamble = many $ (char ':' >> (try parseTest <|> try parseSymlen <|> try parseBase <|>
                                    try parseOutputs <|> try parseSecrets <|> try parseConsts <|>
                                    skipParam))
    lines    = many parseRefLine

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
    addTest (inp, res)
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
    symlens <- many (spaces >> int)
    lift $ zipWithM B.setSymlen [0..] symlens
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

parseConsts :: AcircParser ()
parseConsts = do
    string "consts"
    cs <- many (spaces >> int)
    forM_ (zip [0..] cs) $ \(id, val) -> do
        lift $ B.insertConstVal (Id id) (fromIntegral val)
    endLine

parseRef :: AcircParser Ref
parseRef = do
    ref <- int
    lift $ B.bs_circ . circ_maxref %= max ref
    return (Ref ref)

parseRefLine :: AcircParser ()
parseRefLine = do
    ref <- parseRef
    spaces
    choice [parseConst ref, parseInput ref, parseGate ref]
    parseTimesUsed
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

parseTimesUsed :: AcircParser ()
parseTimesUsed = optional $ do
    spaces
    char ':'
    spaces
    void int <|> void (string "inf") -- times used annotation
