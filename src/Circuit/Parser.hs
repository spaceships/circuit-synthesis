{-# LANGUAGE BangPatterns #-}

module Circuit.Parser where

import Circuit
import qualified Circuit.Builder as B
import qualified Circuit.Builder.Internals as B

import Control.Monad
import Control.Monad.Trans (lift)
import Control.Monad.Identity
import Lens.Micro.Platform
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.IntMap as IM
import qualified Control.Monad.State.Strict as S

type ParseCircT g s m = ParsecT String ([TestCase], s) (B.BuilderT g m)
type ParseCirc g s = ParseCircT g s Identity

addTest :: Monad m => TestCase -> ParseCircT g s m ()
addTest t = modifyState (over _1 (t:))

runCircParser :: s -> ParseCirc g s a -> String -> (Circuit g, [TestCase])
runCircParser st p s = runIdentity $ runCircParserT st p s

runCircParserT :: Monad m => s -> ParseCircT g s m a -> String -> m (Circuit g, [TestCase])
runCircParserT initialState p str = do
    (c, maybeSt) <- B.runCircuitT $ runParserT (p >> getState) ([],initialState) "" str
    case maybeSt of
        Left err -> error (show err)
        Right (ts,_) -> return (c, reverse ts)

execCircParser :: s -> ParseCirc g s a -> String -> (Circuit g, [TestCase], s)
execCircParser st p str = runIdentity $ execCircParserT st p str

execCircParserT :: Monad m => s -> ParseCircT g s m a -> String -> m (Circuit g, [TestCase], s)
execCircParserT initialState p str = do
    (c, maybeSt) <- B.runCircuitT $ runParserT (p >> getState) ([],initialState) "" str
    case maybeSt of
        Left err -> error (show err)
        Right (ts,s) -> return (c, reverse ts, s)

--------------------------------------------------------------------------------
-- custom parsers

oneOfStr :: Monad m => [String] -> ParseCircT g s m String
oneOfStr = foldr (\s m -> string s <|> m) (fail "no strings")

spaces :: Monad m => ParseCircT g s m ()
spaces = skipMany (oneOf " \t")

endLine :: Monad m => ParseCircT g s m ()
endLine = eof <|> void endOfLine

int :: Monad m => ParseCircT g s m Int
int = read <$> many1 digit

--------------------------------------------------------------------------------
-- state manipulation

getSt :: Monad m => ParseCircT g s m s
getSt = snd <$> getState

modifySt :: Monad m => (s -> s) -> ParseCircT g s m ()
modifySt f = modifyState (over _2 f)

refUpdate :: Monad m => Int -> a -> ParseCircT g (IM.IntMap a) m ()
refUpdate !key !val = modifySt (IM.insert key val)

refMerge :: Monad m => IM.IntMap a -> ParseCircT g (IM.IntMap a) m ()
refMerge !map = modifySt (IM.union map)

refLookup :: Monad m => Int -> ParseCircT g (IM.IntMap a) m a
refLookup !key = (IM.! key) <$> getSt

refLookupSafe :: Monad m => Int -> ParseCircT g (IM.IntMap a) m (Maybe a)
refLookupSafe !key = IM.lookup key <$> getSt
