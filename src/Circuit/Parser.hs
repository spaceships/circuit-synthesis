module Circuit.Parser where

import Circuit
import qualified Circuit.Builder as B
import qualified Circuit.Builder.Internals as B

import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Identity
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Data.Set as Set

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

markPublicConst :: Monad m => Ref -> ParseCircT m ()
markPublicConst ref = do
    c <- lift B.getCirc
    case Map.lookup ref (circ_secret_refs c) of
        Nothing -> error ("no const with ref " ++ show ref)
        Just id -> lift $ B.modifyCirc (\c -> c { circ_consts    = Bimap.insert (getSecret c id) ref (circ_consts c)
                                                , circ_const_ids = Set.insert id (circ_const_ids c)
                                                })

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
