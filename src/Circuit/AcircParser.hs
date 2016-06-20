module Circuit.AcircParser
  ( parseCirc
  ) where

import Circuit
import Circuit.Parser
import Util (readBitstring, safeInsert)

import Control.Monad (when)
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map as M

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
    inps <- many (oneOf ['0','1'])
    spaces
    outs <- many (oneOf ['0','1'])
    let inp = readBitstring inps
        res = readBitstring outs
    addTest (inp, res)
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
    when (gateType == "output") $ do
        markOutput ref
    spaces
    opType <- oneOfStr ["ADD", "SUB", "MUL"]
    spaces
    xref <- Ref <$> read <$> ((:) <$> option ' ' (char '-') <*> many1 digit)
    spaces
    yref <- Ref <$> read <$> many1 digit
    let op = case opType of
            "ADD" -> Add xref yref
            "MUL" -> Mul xref yref
            "SUB" -> Sub xref yref
            g     -> error ("[parser] unkonwn gate type " ++ g)
    insertOp ref op
    endLine
