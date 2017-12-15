module Circuit.Parser where

import Circuit
import qualified Circuit.Builder as B
import qualified Circuit.Builder.Internals as B

import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Identity
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map as Map
import qualified Data.IntSet as IntSet

type ParseCircT m = ParsecT String [TestCase] (B.BuilderT m)
type ParseCirc = ParsecT String [TestCase] B.Builder

addTest :: Monad m => TestCase -> ParseCircT m ()
addTest t = modifyState (t:)

runCircParser :: ParseCirc a -> String -> (Circuit, [TestCase])
runCircParser p s = runIdentity $ runCircParserT p s

runCircParserT :: Monad m => ParseCircT m a -> String -> m (Circuit, [TestCase])
runCircParserT p s = do
    (c, maybeTests) <- B.runCircuitT $ runParserT (p >> getState) [] "" s
    case maybeTests of
        Left err -> error (show err)
        Right ts -> return (c, reverse ts)

--------------------------------------------------------------------------------
-- custom parsers

oneOfStr :: Monad m => [String] -> ParseCircT m String
oneOfStr = foldr (\s m -> string s <|> m) (fail "no strings")

spaces :: Monad m => ParseCircT m ()
spaces = skipMany (oneOf " \t")

endLine :: Monad m => ParseCircT m ()
endLine = do
    skipMany (char ' ')
    eof <|> void endOfLine
    return ()

int :: Monad m => ParseCircT m Int
int = read <$> many1 digit
