module Circuit.Parser where

import Circuit
import Util

import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map as M

type CircuitParser = String -> (Circuit, [TestCase])

data ParseSt = ParseSt {
      st_circ   :: Circuit
    , st_tests  :: [TestCase]
    , st_ys     :: M.Map Id Int
    , st_refmap :: M.Map String Ref
    }

emptySt = ParseSt emptyCirc [] M.empty M.empty

type ParseCirc = Parsec String ParseSt

getCirc :: ParseCirc Circuit
getCirc = st_circ <$> getState

modifyCirc :: (Circuit -> Circuit) -> ParseCirc ()
modifyCirc f = modifyState (\st -> st { st_circ = f (st_circ st) })

addTest :: TestCase -> ParseCirc ()
addTest t = modifyState (\st -> st { st_tests = t : st_tests st})

insertOp :: Ref -> Op -> ParseCirc ()
insertOp ref op = do
    refs <- circ_refmap <$> getCirc
    if M.member ref refs
        then error ("redefinition of ref " ++ show ref)
        else modifyCirc (\c -> c { circ_refmap = M.insert ref op refs })

insertConst :: Ref -> Id -> ParseCirc ()
insertConst ref id = do
    refs <- circ_consts <$> getCirc
    let circ_consts' = safeInsert ("redefinition of y" ++ show id) id ref refs
    modifyCirc (\c -> c { circ_consts = circ_consts' })
    insertOp ref (OpConst id)

insertSecret :: Id -> Int -> ParseCirc ()
insertSecret id val = do
    ys <- st_ys <$> getState
    let ys' = safeInsert ("reassignment of y" ++ show id) id val ys
    modifyState (\st -> st { st_ys = ys' })

insertInput :: Ref -> Id -> ParseCirc ()
insertInput ref id = do
    refs <- circ_inputs <$> getCirc
    let circ_inputs' = safeInsert ("redefinition of x" ++ show id) id ref refs
    modifyCirc (\c -> c { circ_inputs = circ_inputs' })
    insertOp ref (OpInput id)

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
