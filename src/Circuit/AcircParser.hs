module Circuit.AcircParser
  ( parseCirc
  ) where

import Circuit
import Circuit.Parser
import Util (readBitstring)

import Control.Monad (when)
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map as M

parseCirc :: String -> (Circuit, [TestCase])
parseCirc s = case runParser (circParser >> getState) emptySt "" s of
    Left err -> error (show err)
    Right st -> let ys = map snd $ M.toAscList (st_ys st)
                in ((st_circ st) { circ_consts = ys }, reverse (st_ts st))
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
    gateRef <- read <$> many1 digit
    spaces
    string "input"
    spaces
    parseX gateRef <|> parseY gateRef
    endLine

parseX :: Ref -> ParseCirc ()
parseX ref = do
    char 'x'
    inpId <- read <$> many1 digit
    insertOp ref (Input inpId)
    refs <- circ_inprefs <$> getCirc
    let circ_inprefs' = safeInsert ("redefinition of x" ++ show inpId) inpId ref refs
    modifyCirc (\c -> c { circ_inprefs = circ_inprefs' })

parseY :: Ref -> ParseCirc ()
parseY ref = do
    char 'y'
    inpId <- read <$> many1 digit
    spaces
    val <- read <$> many1 digit
    insertOp ref (Const inpId)
    insertConst inpId val

parseGate :: ParseCirc ()
parseGate = do
    ref <- read <$> many1 digit
    spaces
    gateType <- oneOfStr ["gate", "output"]
    when (gateType == "output") $ do
        markOutput ref
    spaces
    opType <- oneOfStr ["ADD", "SUB", "MUL"]
    spaces
    xref <- read <$> ((:) <$> option ' ' (char '-') <*> many1 digit)
    spaces
    yref <- read <$> many1 digit
    let op = case opType of
            "ADD" -> Add xref yref
            "MUL" -> Mul xref yref
            "SUB" -> Sub xref yref
            g     -> error ("[parser] unkonwn gate type " ++ g)
    insertOp ref op
    endLine
