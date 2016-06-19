{-# LANGUAGE FlexibleContexts #-}

module Circuit.Builder where

import Circuit

import Control.Monad.State
import qualified Data.Map as M


data BuildSt = BuildSt {
      st_circ   :: Circuit
    , st_ys     :: M.Map ID Integer
    , st_refmap :: M.Map String Ref
    }

emptyBuild :: BuildSt
emptyBuild = BuildSt emptyCirc M.empty M.empty

getCirc :: MonadState BuildSt m => m Circuit
getCirc = st_circ <$> get

modifyCirc :: MonadState BuildSt m => (Circuit -> Circuit) -> m ()
modifyCirc f = modify (\st -> st { st_circ = f (st_circ st) })

insertConst :: MonadState BuildSt m => ID -> Integer -> m ()
insertConst i c = modify (\st -> st { st_ys = M.insert i c (st_ys st)})

insertOp :: MonadState BuildSt m => Ref -> Op -> m ()
insertOp ref op = do
    refs <- refMap <$> getCirc
    if M.member ref refs
        then error ("redefinition of ref " ++ show ref)
        else modifyCirc (\c -> c { refMap = M.insert ref op refs })


--------------------------------------------------------------------------------
-- smart constructors


buildCircuit :: State BuildSt Circuit -> Circuit
