module Circuit.Acirc
  ( parseCirc
  , showCirc
  , showTest
  ) where

import Circuit
import Circuit.Parser
import Util (readBitstring, showBitstring', safeInsert)

import Control.Monad (when)
import Control.Monad.Writer
import Text.Parsec hiding (spaces, parseTest)
import Text.Printf
import qualified Data.Set as S
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- printer

showCirc :: Circuit -> String
showCirc c = unlines (header ++ inputs ++ consts ++ gates ++ output)
  where
    header = [printf ": nins %d" (ninputs c), printf ": depth %d" (depth c)]
    inputs = map (gateStr' False) (circ_inputs c)
    consts = map (gateStr' False) (circ_consts c)
    output = map (gateStr' True)  (circ_outputs c)
    gates  = execWriter (foldCircM eval c)

    eval :: Op -> Ref -> [()] -> Writer [String] ()
    eval (OpInput _) _ _ = return ()
    eval (OpConst _) _ _ = return ()
    eval op ref _ = if ref `elem` circ_outputs c
                       then return ()
                       else tell [gateStr ref op False]

    gateStr' :: Bool -> Ref -> String
    gateStr' isOutput ref = case M.lookup ref (circ_refmap c) of
        Nothing -> error (printf "[gateStr'] unknown ref %s" (show ref))
        Just op -> gateStr ref op isOutput

    gateStr :: Ref -> Op -> Bool -> String
    gateStr ref (OpInput id) _ = printf "%d input x%d" (getRef ref) (getId id)
    gateStr ref (OpConst id) _ = let secret = case M.lookup id (circ_secrets c) of
                                                Nothing -> ""
                                                Just y  -> show y
                                 in printf "%d input y%d %s" (getRef ref) (getId id) (secret)

    gateStr ref op isOutput  = printf "%d %s %s %d %d"
                                    (getRef ref)
                                    (if isOutput then "output" else "gate")
                                    ty (getRef x) (getRef y)
        where
            (ty, x, y) = case op of OpAdd x y -> ("ADD", x, y)
                                    OpSub x y -> ("SUB", x, y)
                                    OpMul x y -> ("MUL", x, y)
                                    _ -> error "[gateStr] input and const handled elsewhere"


showTest :: TestCase -> String
showTest (inp, out) = printf "# TEST %s %s" (showBitstring' inp) (showBitstring' out)

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
            "ADD" -> OpAdd xref yref
            "MUL" -> OpMul xref yref
            "SUB" -> OpSub xref yref
            g     -> error ("[parser] unkonwn gate type " ++ g)
    insertOp ref op
    endLine
