{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Circuit.Builder.Internals where

import Circuit
import Circuit.Utils

import Control.Monad.State.Strict
import Control.Monad.Identity
import Lens.Micro.Platform
import Text.Printf
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as S

type BuilderT = StateT BuildSt
type Builder = BuilderT Identity

data BuildSt = BuildSt
    { _bs_circ        :: !Circuit
    , _bs_next_ref    :: !Ref
    , _bs_next_inp    :: !Id
    , _bs_next_const  :: !Id
    , _bs_dedup       :: !(M.Map Op Ref)
    , _bs_constants   :: !(M.Map Integer Ref)
    }

makeLenses ''BuildSt

emptyBuild :: BuildSt
emptyBuild = BuildSt emptyCirc 0 0 0 M.empty M.empty

runCircuitT :: Monad m => BuilderT m a -> m (Circuit, a)
runCircuitT b = do
    (a, st) <- runStateT b emptyBuild
    return (st^.bs_circ, a)

buildCircuitT :: Monad m => BuilderT m a -> m Circuit
buildCircuitT b = view bs_circ <$> execStateT b emptyBuild

runCircuit :: Builder a -> (Circuit, a)
runCircuit b = runIdentity (runCircuitT b)

buildCircuit :: Builder a -> Circuit
buildCircuit = view bs_circ . flip execState emptyBuild

--------------------------------------------------------------------------------
-- operations

setSymlen :: Monad m => Int -> BuilderT m ()
setSymlen !n = bs_circ . circ_symlen .= n

setBase :: Monad m => Int -> BuilderT m ()
setBase !n = bs_circ . circ_base .= n

insertOp :: Monad m => Ref -> Op -> BuilderT m ()
insertOp !ref !op = do
    refs <- use $ bs_circ . circ_refmap
    when (IM.member (getRef ref) refs) $
        error ("redefinition of ref " ++ show ref)
    bs_circ . circ_refmap . at (getRef ref) ?= op
    bs_dedup . at op ?= ref

insertConst :: Monad m => Ref -> Id -> BuilderT m ()
insertConst !ref !id = do
    bs_circ . circ_consts . at ref ?= id
    insertOp ref (OpConst id)

insertConstVal :: Monad m => Id -> Integer -> BuilderT m ()
insertConstVal !id !val = do
    ys <- use $ bs_circ . circ_const_vals
    let ys' = safeInsert ("reassignment of y" ++ show id) id val ys
    bs_circ . circ_const_vals .= ys'

insertInput :: Monad m => Ref -> Id -> BuilderT m ()
insertInput !ref !id = do
    bs_circ . circ_inputs %= (++[ref])
    insertOp ref (OpInput id)

newOp :: Monad m => Op -> BuilderT m Ref
newOp !op = do
    dedup <- use bs_dedup
    case M.lookup op dedup of
        Nothing -> do
            ref <- nextRef
            insertOp ref op
            return ref
        Just ref -> do
            return ref

nextRef :: Monad m => BuilderT m Ref
nextRef = do
    ref <- use bs_next_ref
    bs_next_ref += 1
    return ref

nextInputId :: Monad m => BuilderT m Id
nextInputId = do
    id <- use bs_next_inp
    bs_next_inp += 1
    return id

nextConstId :: Monad m => BuilderT m Id
nextConstId = do
    id <- use bs_next_const
    bs_next_const += 1
    return id

markOutput :: Monad m => Ref -> BuilderT m ()
markOutput !ref = bs_circ . circ_outputs %= (++[ref])

markSecret :: Monad m => Ref -> BuilderT m ()
markSecret !ref = do
    id <- use $ bs_circ . circ_consts . at ref
    case id of
        Nothing  -> error $ printf "[markSecret] ref %s is not a const!" (show ref)
        Just id' -> bs_circ . circ_secrets . at ref ?= id'

markConstant :: Monad m => Integer -> Ref -> BuilderT m ()
markConstant !x !ref = bs_constants . at x ?= ref

existingConstant :: Monad m => Integer -> BuilderT m (Maybe Ref)
existingConstant !x = gets (M.lookup x . _bs_constants)
