module Circuit.Format.Verilog
  where

import Circuit
import Circuit.Parser

import Control.Monad
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map as M

fromFile :: FilePath -> IO (Circuit, [TestCase])
fromFile fp = parseCirc <$> readFile fp

parseCirc :: String -> (Circuit, [TestCase])
parseCirc s = case runParser (circParser >> eof >> getState) emptySt "" s of
    Left err -> error (show err)
    Right st -> (st_circ st, [])
  where
    circParser = many $ choice [ void newline
                               , ignoreLine "(*"
                               , ignoreLine "/*"
                               , ignoreLine "module"
                               , ignoreLine "wire"
                               ]

skipComment :: ParseCirc ()
skipComment = void ((type1 <|> type2) >> newline)
  where
    type1 = do
        string "/*"
        manyTill anyChar (string "*/")
    type2 = do
        string "(*"
        manyTill anyChar (string "*)")

ignoreLine :: String -> ParseCirc ()
ignoreLine str = try $ do
    spaces
    string str
    void $ manyTill anyChar newline

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
