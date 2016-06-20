module Circuit.Parser where

import Circuit

import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map as M

type CircuitParser = String -> (Circuit, [TestCase])

data ParseSt = ParseSt {
      st_circ   :: Circuit
    , st_ts     :: [TestCase]
    , st_ys     :: M.Map ID Integer
    , st_refmap :: M.Map String Ref
    }

emptySt = ParseSt emptyCirc [] M.empty M.empty

type ParseCirc = Parsec String ParseSt

getCirc :: ParseCirc Circuit
getCirc = st_circ <$> getState

modifyCirc :: (Circuit -> Circuit) -> ParseCirc ()
modifyCirc f = modifyState (\st -> st { st_circ = f (st_circ st) })

addTest :: TestCase -> ParseCirc ()
addTest t = modifyState (\st -> st { st_ts = t : st_ts st})

safeInsert :: Ord a => String -> a -> b -> M.Map a b -> M.Map a b
safeInsert errorMsg x y m =
    if M.member x m
       then error errorMsg
       else M.insert x y m

insertConst :: ID -> Integer -> ParseCirc ()
insertConst i c = modifyState (\st -> st { st_ys = M.insert i c (st_ys st)})

insertOp :: Ref -> Op -> ParseCirc ()
insertOp ref op = do
    refs <- circ_refmap <$> getCirc
    if M.member ref refs
        then error ("redefinition of ref " ++ show ref)
        else modifyCirc (\c -> c { circ_refmap = M.insert ref op refs })

markOutput :: Ref -> ParseCirc ()
markOutput ref = modifyCirc (\c -> c { circ_outrefs = circ_outrefs c ++ [ref] })

--------------------------------------------------------------------------------
-- custom parsers

oneOfStr :: [String] -> ParseCirc String
oneOfStr = foldr (\s m -> string s <|> m) (fail "no strings")

spaces :: ParseCirc ()
spaces = skipMany (oneOf " \t")

endLine :: ParseCirc ()
endLine = do
    skipMany (char ' ')
    eof <|> (endOfLine >> return ())
    return ()
