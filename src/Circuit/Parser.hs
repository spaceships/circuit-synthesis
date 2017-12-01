module Circuit.Parser where

import Circuit
import qualified Circuit.Builder as B

import Control.Monad
import Control.Monad.Trans (lift)
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map as Map
import qualified Data.Bimap as Bimap
import qualified Data.Set as Set

type ParseCirc = ParsecT String [TestCase] B.Builder

addTest :: TestCase -> ParseCirc ()
addTest t = modifyState (t:)

runCircParser :: ParseCirc a -> String -> (Circuit, [TestCase])
runCircParser p s =
    let (c, maybeTests) = B.runCircuit $ runParserT (p >> getState) [] "" s
    in case maybeTests of
        Left err -> error (show err)
        Right ts -> (c, reverse ts)

markPublicConst :: Ref -> ParseCirc ()
markPublicConst ref = do
    c <- lift B.getCirc
    case Map.lookup ref (circ_secret_refs c) of
        Nothing -> error ("no const with ref " ++ show ref)
        Just id -> lift $ B.modifyCirc (\c -> c { circ_consts    = Bimap.insert (getSecret c id) ref (circ_consts c)
                                                , circ_const_ids = Set.insert id (circ_const_ids c)
                                                })

--------------------------------------------------------------------------------
-- custom parsers

oneOfStr :: [String] -> ParseCirc String
oneOfStr = foldr (\s m -> string s <|> m) (fail "no strings")

spaces :: ParseCirc ()
spaces = skipMany (oneOf " \t")

endLine :: ParseCirc ()
endLine = do
    skipMany (char ' ')
    eof <|> void endOfLine
    return ()
