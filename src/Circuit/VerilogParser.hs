module Circuit.VerilogParser
  where

import Circuit
import Circuit.Parser

import Control.Monad
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map as M

parseCirc :: String -> (Circuit, [TestCase])
parseCirc s = case runParser (circParser >> getState) emptySt "" s of
    Left err -> error (show err)
    Right st -> (st_circ st, [])
  where
    circParser = skipComment
    {-start = many $ choice [parseParam, parseTest]-}
    {-rest  = many $ choice [try parseGate, try parseInput]-}

skipComment :: ParseCirc ()
skipComment = void (type1 <|> type2)
  where
    type1 = do
        string "/*"
        manyTill anyChar (string "*/")
    type2 = do
        string "(*"
        manyTill anyChar (string "*)")

parseModule :: ParseCirc ()
parseModule = do
    string "module"
    spaces

parseWire = undefined

parseParam :: ParseCirc ()
parseParam = do
    char ':'
    skipMany (oneOf " \t" <|> alphaNum)
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

parseY = undefined
{-parseY :: Ref -> ParseCirc ()-}
{-parseY ref = do-}
    {-char 'y'-}
    {-id <- Id <$> read <$> many1 digit-}
    {-spaces-}
    {-val <- read <$> many1 digit-}
    {-insertConst ref id-}

{-parseGate :: ParseCirc ()-}
{-parseGate = do-}
    {-ref <- read <$> many1 digit-}
    {-spaces-}
    {-gateType <- oneOfStr ["gate", "output"]-}
    {-when (gateType == "output") $ do-}
        {-c <- getCirc-}
        {-if outRef c > 0 then-}
            {-error ("multiple outputs defined! ref" ++ show ref)-}
        {-else-}
            {-modifyCirc (\c' -> c' { outRef = ref })-}
    {-spaces-}
    {-opType <- oneOfStr ["ADD", "SUB", "MUL"]-}
    {-spaces-}
    {-xref <- read <$> ((:) <$> option ' ' (char '-') <*> many1 digit)-}
    {-spaces-}
    {-yref <- read <$> many1 digit-}
    {-let op = case opType of-}
            {-"ADD" -> Add xref yref-}
            {-"MUL" -> Mul xref yref-}
            {-"SUB" -> Sub xref yref-}
            {-g     -> error ("[parser] unkonwn gate type " ++ g)-}
    {-insertOp ref op-}
    {-endLine-}
