module Circuit.Parser where

import Circuit
import qualified Circuit.Builder as B
import qualified Circuit.Builder.Internals as B

import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Identity
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.IntMap as IM
import qualified Control.Monad.State.Strict as S

type ParseCircT g m = ParsecT String [TestCase] (B.BuilderT g (S.StateT (IM.IntMap Ref) m))
type ParseCirc g = ParseCircT g Identity

addTest :: Monad m => TestCase -> ParseCircT g m ()
addTest t = modifyState (t:)

runCircParser :: ParseCirc g a -> String -> (Circuit g, [TestCase])
runCircParser p s = runIdentity $ runCircParserT p s

runCircParserT :: Monad m => ParseCircT g m a -> String -> m (Circuit g, [TestCase])
runCircParserT p s = flip S.evalStateT IM.empty $ do
    (c, maybeTests) <- B.runCircuitT $ runParserT (p >> getState) [] "" s
    case maybeTests of
        Left err -> error (show err)
        Right ts -> return (c, reverse ts)

--------------------------------------------------------------------------------
-- custom parsers

oneOfStr :: Monad m => [String] -> ParseCircT g m String
oneOfStr = foldr (\s m -> string s <|> m) (fail "no strings")

spaces :: Monad m => ParseCircT g m ()
spaces = skipMany (oneOf " \t")

endLine :: Monad m => ParseCircT g m ()
endLine = do
    skipMany (char ' ')
    eof <|> void endOfLine
    return ()

int :: Monad m => ParseCircT g m Int
int = read <$> many1 digit

--------------------------------------------------------------------------------
-- state manipulation

refUpdate :: Monad m => Int -> Ref -> ParseCircT g m ()
refUpdate existingRef newRef = (lift.lift) $ S.modify (IM.insert existingRef newRef)

refMerge :: Monad m => IM.IntMap Ref -> ParseCircT g m ()
refMerge map = (lift.lift) $ S.modify (IM.union map)

refLookup :: Monad m => Int -> ParseCircT g m Ref
refLookup nigelRef = (lift.lift) $ S.gets (IM.! nigelRef)
