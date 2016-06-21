module Circuit.Parser where

import Circuit
import Util

import Control.Monad
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map as M

type CircuitParser = String -> (Circuit, [TestCase])

data ParseSt = ParseSt {
      st_circ   :: Circuit
    , st_tests  :: [TestCase]
    , st_refmap :: M.Map String Ref
    }

emptySt = ParseSt emptyCirc [] M.empty

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
    modifyCirc (\c -> c { circ_consts = circ_consts c ++ [ref] })
    insertOp ref (OpConst id)

insertSecret :: Id -> Integer -> ParseCirc ()
insertSecret id val = do
    ys <- circ_secrets <$> getCirc
    let ys' = safeInsert ("reassignment of y" ++ show id) id val ys
    modifyCirc (\c -> c { circ_secrets = ys' })

insertInput :: Ref -> Id -> ParseCirc ()
insertInput ref id = do
    modifyCirc (\c -> c { circ_inputs = circ_inputs c ++ [ref] })
    insertOp ref (OpInput id)

markOutput :: Ref -> ParseCirc ()
markOutput ref = modifyCirc (\c -> c { circ_outputs = circ_outputs c ++ [ref] })

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
