module Circuit.Format.Sexp where

import Text.Parsec
import Text.Printf
import qualified Circuit.Builder as B
import Control.Monad
import Control.Monad.Trans
import Circuit

circToSexp :: Acirc -> [String]
circToSexp c = foldCirc eval c
  where
    eval (ArithAdd _ _) [x,y] = printf "Add(%s, %s)" x y
    eval (ArithSub _ _) [x,y] = printf "Add(%s, Mul(Integer(-1), %s))" x y
    eval (ArithMul _ _) [x,y] = printf "Mul(%s, %s)" x y
    eval (ArithBase (Input i)) []   = printf "Symbol('x%d')" (getId i)
    eval (ArithBase (Const i)) []   = if secretConst c i
                                         then printf "Symbol('y%d')" (getId i)
                                         else printf "Integer(%d)" (getConst c i)
    eval op args  = error ("[circToSexp] weird input: " ++ show op ++ " " ++ show args)

type SexpParser = ParsecT String () (B.Builder ArithGate)

parse :: Int -> String -> Circuit ArithGate
parse ninps s = B.buildCircuit $ do
    _   <- B.inputs ninps
    res <- runParserT parseSexp () "" s
    case res of
        (Left e) -> error (show e)
        (Right ref) -> B.output ref

parseSexp :: SexpParser Ref
parseSexp = try parseAdd <|> try parseSub <|> try parseMul <|> try parseInput
                         <|> try parseConst <|> parseInteger <?> "sexp"

parseSub :: SexpParser Ref
parseSub = do
    _ <- string "Sub("
    x <- parseSexp
    spaces
    _ <- string ","
    spaces
    y <- parseSexp
    _ <- string ")"
    lift (B.circSub x y)

parseAdd :: SexpParser Ref
parseAdd = do
    _ <- string "Add("
    args <- parseSexp `sepBy` string ", "
    when (length args < 2) $
        (unexpected "gate requires 2 or more arguments")
    _ <- string ")"
    lift (B.circSum args)

-- parseNegate :: SexpParser Ref
-- parseNegate = do
--     _ <- string "Mul(Integer(-1), "
--     x <- parseSexp
--     _ <- string ")"
--     y <- lift (B.constant 1)
--     lift (B.circSub y x)

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
    lift (B.input_n (Id n))

parseConst :: SexpParser Ref
parseConst = do
    _ <- string "Symbol('y"
    n <- read <$> many digit
    _ <- string "')"
    lift (B.secret_n (Id n))

parseInteger :: SexpParser Ref
parseInteger = do
    _ <- string "Integer("
    n <- read <$> many (digit <|> char '-')
    _ <- string ")"
    lift (B.constant n)
