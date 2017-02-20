module Circuit.Format.Sexp where

import Text.Parsec
import qualified Circuit.Builder as B
import Control.Monad
import Control.Monad.Trans
import Circuit

data Sexp = Sexp String [Sexp] | Atom String
          deriving (Eq, Show)

-- ref is the reference to const 1
type SexpParser = ParsecT String Ref B.Builder

-- parse :: [String] -> Circuit
-- parse ss = B.buildCircuit $ do
--     one  <- B.constant 1
--     outs <- mapM (runParserT parseSexp one) ss
--     B.outputs outs

parseSexp :: SexpParser Ref
parseSexp = parseAdd <|> parseNegate <|> parseMul <|> parseInput <|> parseConst

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
    y <- getState -- const 1
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
    lift (B.input_n n)


-- parseSexp :: String -> Builder (Ref
-- parseSexp :: String -> (Sexp, String)
-- parseSexp s = Sexp elem (map parseSexp
--   where
--     op = takeWhile (/= '(') s
--     r0 = tail (dropWhile (/= '('))
--     r1 =
