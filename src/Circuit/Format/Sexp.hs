module Circuit.Format.Sexp where

import Text.Parsec
import qualified Circuit.Builder as B
import Control.Monad
import Control.Monad.Trans
import Circuit
import Data.Either (rights)

data Sexp = Sexp String [Sexp] | Atom String
          deriving (Eq, Show)

type SexpParser = ParsecT String () B.Builder

parse :: [String] -> Circuit
parse ss = B.buildCircuit $ do
    outs <- rights <$> mapM (runParserT parseSexp () "") ss
    B.outputs outs

parseNInputs :: Int -> [String] -> Circuit
parseNInputs n ss = B.buildCircuit $ do
    _    <- B.inputs n
    outs <- rights <$> mapM (runParserT parseSexp () "") ss
    B.outputs outs

parseSexp :: SexpParser Ref
parseSexp = try parseSub <|> try parseAdd <|> try parseNegate <|> try parseMul <|> try parseInput <|> parseConst

parseSub :: SexpParser Ref
parseSub = do
    _ <- string "Add("
    x <- parseSexp
    _ <- string ", Mul(Integer(-1), "
    y <- parseSexp
    _ <- string "))"
    lift (B.circSub x y)

parseAdd :: SexpParser Ref
parseAdd = do
    _ <- string "Add("
    args <- parseSexp `sepBy` string ", "
    when (length args < 2) $
        (unexpected "gate requires 2 or more arguments")
    _ <- string ")"
    lift (B.circSum args)

parseNegate :: SexpParser Ref
parseNegate = do
    _ <- string "Mul(Integer(-1), "
    x <- parseSexp
    _ <- string ")"
    y <- lift (B.constant 1)
    lift (B.circSub y x)

parseMul :: SexpParser Ref
parseMul = do
    _ <- string "Mul("
    args <- parseSexp `sepBy` string ", "
    when (length args < 2) $
        (unexpected "gate requires 2 or more arguments")
    _ <- string ")"
    lift (B.circProd args)

parseInput :: SexpParser Ref
parseInput = do
    _ <- string "Symbol('x"
    n <- read <$> many digit
    _ <- string "')"
    lift (B.input_n n)

parseConst :: SexpParser Ref
parseConst = do
    _ <- string "Symbol('y"
    n <- read <$> many digit
    _ <- string "')"
    lift (B.secret_n n)


-- parseSexp :: String -> Builder (Ref
-- parseSexp :: String -> (Sexp, String)
-- parseSexp s = Sexp elem (map parseSexp
--   where
--     op = takeWhile (/= '(') s
--     r0 = tail (dropWhile (/= '('))
--     r1 =
