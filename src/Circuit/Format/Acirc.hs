module Circuit.Format.Acirc
  ( parseCirc
  , showCirc
  , showTest
  ) where

import Circuit
import Circuit.Parser
import Util (readBits, showBits', safeInsert)

import Control.Monad (when)
import Control.Monad.Writer
import Text.Parsec hiding (spaces, parseTest)
import Text.Printf
import qualified Data.Set as S
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- printer

showCirc :: Circuit -> String
showCirc c = unlines (header ++ inputs ++ consts ++ igates ++ output)
  where
    header = [printf ": nins %d" (ninputs c), printf ": depth %d" (depth c)]
    inputs = map (gateStr False) (circ_inputs c)
    consts = map (gateStr False) (circ_consts c)
    igates = map (gateStr False) (intermediateGates c)
    output = map (gateStr True)  (circ_output c)

    gateStr :: Bool -> Ref -> String
    gateStr isOutput ref = case M.lookup ref (circ_refmap c) of
        Nothing -> error (printf "[gateStr] unknown ref %s" (show ref))
        Just (OpInput id) -> printf "%d input x%d" (getRef ref) (getId id)
        Just (OpConst id) -> let secret = case M.lookup id (circ_secrets c) of
                                            Nothing -> ""
                                            Just y  -> show y
                             in printf "%d input y%d %s" (getRef ref) (getId id) secret
        Just (OpAdd x y) -> pr ref "ADD" x y isOutput
        Just (OpSub x y) -> pr ref "SUB" x y isOutput
        Just (OpMul x y) -> pr ref "MUL" x y isOutput

    pr ref gateTy x y isOutput = printf "%d %s %s %d %d" (getRef ref)
                                        (if isOutput then "output" else "gate")
                                        gateTy (getRef x) (getRef y)


showTest :: TestCase -> String
showTest (inp, out) = printf "# TEST %s %s" (showBits' inp) (showBits' out)

--------------------------------------------------------------------------------
-- parser

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
    inps <- many (oneOf "01")
    spaces
    outs <- many (oneOf "01")
    let inp = readBits inps
        res = readBits outs
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
    when (gateType == "output") $ markOutput ref
    spaces
    opType <- oneOfStr ["ADD", "SUB", "MUL"]
    spaces
    xref <- Ref <$> read <$> ((:) <$> option ' ' (char '-') <*> many1 digit)
    spaces
    yref <- Ref <$> read <$> many1 digit
    let op = case opType of
            "ADD" -> OpAdd xref yref
            "MUL" -> OpMul xref yref
            "SUB" -> OpSub xref yref
            g     -> error ("[parser] unkonwn gate type " ++ g)
    insertOp ref op
    endLine
