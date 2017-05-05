module Circuit.Parser where

import Circuit
import Util

import Control.Monad
import Text.Parsec hiding (spaces, parseTest)
import qualified Data.Map   as M
import qualified Data.Bimap as B
import qualified Data.Set   as S

data ParseSt = ParseSt {
      st_circ   :: Circuit
    , st_tests  :: [TestCase]
    , st_refmap :: M.Map String Ref
    , st_next_const_id :: Int
    }

emptySt :: ParseSt
emptySt = ParseSt emptyCirc [] M.empty 0

type ParseCirc = Parsec String ParseSt

getCirc :: ParseCirc Circuit
getCirc = st_circ <$> getState

modifyCirc :: (Circuit -> Circuit) -> ParseCirc ()
modifyCirc f = modifyState (\st -> st { st_circ = f (st_circ st) })

setSymlen :: Int -> ParseCirc ()
setSymlen n = modifyCirc (\c -> c { circ_symlen = n })

addTest :: TestCase -> ParseCirc ()
addTest t = modifyState (\st -> st { st_tests = t : st_tests st})

insertOp :: Ref -> Op -> ParseCirc ()
insertOp ref op = do
    refs <- circ_refmap <$> getCirc
    if M.member ref refs
        then error ("redefinition of ref " ++ show ref)
        else modifyCirc (\c -> c { circ_refmap = M.insert ref op refs })

insertSecret :: Ref -> Id -> ParseCirc ()
insertSecret ref id = do
    modifyCirc (\c -> c { circ_secret_refs = M.insert ref id (circ_secret_refs c) })
    insertOp ref (OpSecret id)

insertSecretVal :: Id -> Integer -> ParseCirc ()
insertSecretVal id val = do
    ys <- circ_secrets <$> getCirc
    let ys' = safeInsert ("reassignment of y" ++ show id) id val ys
    modifyCirc (\c -> c { circ_secrets = ys' })

insertInput :: Ref -> Id -> ParseCirc ()
insertInput ref id = do
    modifyCirc (\c -> c { circ_inputs = circ_inputs c ++ [ref] })
    insertOp ref (OpInput id)

markOutput :: Ref -> ParseCirc ()
markOutput ref = modifyCirc (\c -> c { circ_outputs = circ_outputs c ++ [ref] })

markPublicConst :: Ref -> ParseCirc ()
markPublicConst ref = do
    c <- getCirc
    case M.lookup ref (circ_secret_refs c) of
        Nothing -> error ("no const with ref " ++ show ref)
        Just id -> modifyCirc (\c -> c { circ_consts    = B.insert (getSecret c id) ref (circ_consts c)
                                       , circ_const_ids = S.insert id (circ_const_ids c)
                                       })

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
