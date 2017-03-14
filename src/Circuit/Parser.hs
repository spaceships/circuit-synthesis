module Circuit.Parser where

import Circuit
import Types
import Circuit.TypeCheck
import Util

import Control.Monad
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map as M

type CircuitParser = String -> (Circuit, [TestCase])

data Version = Int :.: Int
  deriving (Show, Eq)

vZero :: Version
vZero = 0 :.: 0

data ParseSt = ParseSt {
      st_circ   :: Circuit
    , st_tests  :: [TestCase]
    , st_refmap :: M.Map String Ref
    , st_next_const_id :: Int
    , st_ver    :: Version
    }

emptySt = ParseSt emptyCirc [] M.empty 0 vZero

type ParseCirc = Parsec String ParseSt

getCirc :: ParseCirc Circuit
getCirc = st_circ <$> getState

getVersion :: ParseCirc Version
getVersion = st_ver <$> getState

modifyCirc :: (Circuit -> Circuit) -> ParseCirc ()
modifyCirc f = modifyState (\st -> st { st_circ = f (st_circ st) })

addTest :: TestCase -> ParseCirc ()
addTest t = modifyState (\st -> st { st_tests = t : st_tests st})

insertOpType :: Ref -> Op -> MType -> ParseCirc ()
insertOpType ref op t = do
    refs <- circ_refmap <$> getCirc
    if M.member ref refs
        then error ("redefinition of ref " ++ show ref)
        else modifyCirc (\c -> c { circ_refmap = M.insert ref (op,t) refs })

insertOp :: Ref -> Op -> ParseCirc ()
insertOp ref op = getCirc >>= insertOpType ref op . typeOfOp op


insertSecret :: Ref -> Id -> ParseCirc ()
insertSecret ref id = do
    modifyCirc (\c -> c { circ_secret_refs = M.insert ref id (circ_secret_refs c) })
    insertOp ref (OpSecret id)

insertSecretVal :: Id -> Integer -> ParseCirc ()
insertSecretVal id val = do
    ys <- circ_secrets <$> getCirc
    let ys' = safeInsert ("reassignment of y" ++ show id) id val ys
    modifyCirc (\c -> c { circ_secrets = ys' })

insertInputType :: Ref -> Id -> MType -> ParseCirc ()
insertInputType ref id t = do
    modifyCirc (\c -> c { circ_inputs = circ_inputs c ++ [ref] })
    modifyCirc (\c -> c { circ_input_type = circ_input_type c ++ [t] })
    insertOp ref (OpInput id t)

insertInput :: Ref -> Id -> ParseCirc ()
insertInput r i = insertInputType r i Nothing

markOutput :: Ref -> ParseCirc ()
markOutput ref = modifyCirc (\c -> let (_,t) = circ_refmap c M.! ref in c { circ_outputs = circ_outputs c ++ [(ref,t)] })

nextConstId :: ParseCirc Id
nextConstId = do
    id <- st_next_const_id <$> getState
    modifyState (\st -> st { st_next_const_id = id + 1 })
    return (Id id)

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
