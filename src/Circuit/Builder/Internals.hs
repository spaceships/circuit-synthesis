{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Circuit.Builder.Internals where

import Circuit

import Control.Monad.State.Strict
import Control.Monad.Identity
import Lens.Micro.Platform
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V

type BuilderT g = StateT (BuildSt g)
type Builder g = BuilderT g Identity

data BuildSt g = BuildSt
    { _bs_circ        :: !(Circuit g)
    , _bs_next_inp    :: !InputId
    , _bs_next_const  :: !ConstId
    , _bs_next_secret :: !SecretId
    , _bs_next_sym    :: !Int
    , _bs_dedup       :: !(M.Map g Ref)
    , _bs_constants   :: !(M.Map Int Ref)
    }

makeLenses ''BuildSt

emptyBuild :: BuildSt g
emptyBuild = BuildSt emptyCirc 0 0 0 0 M.empty M.empty

--  TODO: if circ_refsave is on, find the refs that can be added to circ_refskip
runCircuitT :: Monad m => BuilderT g m a -> m (Circuit g, a)
runCircuitT b = do
    (a, st) <- runStateT b emptyBuild
    return (st^.bs_circ, a)

buildCircuitT :: Monad m => BuilderT g m a -> m (Circuit g)
buildCircuitT b = view bs_circ <$> execStateT b emptyBuild

runCircuit :: Builder g a -> (Circuit g, a)
runCircuit b = runIdentity (runCircuitT b)

buildCircuit :: Builder g a -> Circuit g
buildCircuit = view bs_circ . flip execState emptyBuild

--------------------------------------------------------------------------------
-- operations

setSymlen :: Monad m => SymId -> Int -> BuilderT g m ()
setSymlen !id !n = bs_circ . circ_symlen . at (getSymId id) ?= n

setSigma :: Monad m => SymId -> BuilderT g m ()
setSigma !id = bs_circ . circ_sigma_vecs %= IS.insert (getSymId id)

insertGate :: (Gate g, Ord g, Monad m) => Ref -> g -> BuilderT g m ()
insertGate !ref !gate = do
    refs <- use $ bs_circ . circ_refmap
    when (IM.member (getRef ref) refs) $
        error ("redefinition of ref " ++ show ref)
    bs_circ . circ_refmap . at (getRef ref) ?= gate
    bs_dedup . at gate ?= ref
    mapM_ bumpRefCount (gateArgs gate)

insertConst :: (Gate g, Monad m) => Ref -> ConstId -> BuilderT g m ()
insertConst !ref !id = do
    bs_circ . circ_consts . at (getConstId id) ?= ref
    insertGate ref (gateBase (Const id))

insertSecret :: (Gate g, Monad m) => Ref -> SecretId -> BuilderT g m ()
insertSecret !ref !id = do
    bs_circ . circ_secrets . at (getSecretId id) ?= ref
    insertGate ref (gateBase (Secret id))

insertConstVal :: Monad m => ConstId -> Int -> BuilderT g m ()
insertConstVal !id !val = bs_circ . circ_const_vals . at (getConstId id) ?= val

insertSecretVal :: Monad m => SecretId -> Int -> BuilderT g m ()
insertSecretVal !id !val = bs_circ . circ_secret_vals . at (getSecretId id) ?= val

insertInput :: (Gate g, Monad m) => Ref -> InputId -> BuilderT g m ()
insertInput !ref !id = do
    bs_circ . circ_inputs . at (getInputId id) ?= ref
    insertGate ref (gateBase (Input id))

newGate :: (Ord g, Monad m, Gate g) => g -> BuilderT g m Ref
newGate !gate = do
    dedup <- use bs_dedup
    case M.lookup gate dedup of
        Nothing -> do
            ref <- nextRef
            insertGate ref gate
            return ref
        Just ref -> do
            return ref

nextRef :: Monad m => BuilderT g m Ref
nextRef = do
    ref <- use (bs_circ . circ_maxref)
    bs_circ . circ_maxref += 1
    return (Ref ref)

nextInputId :: Monad m => BuilderT g m InputId
nextInputId = do
    id <- use bs_next_inp
    bs_next_inp += 1
    return id

nextConstId :: Monad m => BuilderT g m ConstId
nextConstId = do
    id <- use bs_next_const
    bs_next_const += 1
    return id

nextSecretId :: Monad m => BuilderT g m SecretId
nextSecretId = do
    id <- use bs_next_secret
    bs_next_secret += 1
    return id

nextSymbol :: Monad m => BuilderT g m (SymId)
nextSymbol = do
    i <- use bs_next_sym
    bs_next_sym += 1
    return (SymId i)

bumpRefCount :: Monad m => Ref -> BuilderT g m ()
bumpRefCount ref = bs_circ . circ_refcount %= IM.insertWith (+) (getRef ref) 1

-- XXX: marks everything in the circuit before this ref as Skip
saveRef :: (Gate g, Monad m) => Ref -> BuilderT g m ()
saveRef ref = do
    markSave ref
    c <- use bs_circ
    mapM_ skipRec (gateArgs (getGate c ref))
  where
    skipRec ref = do
        markSkip ref
        g <- flip getGate ref <$> use bs_circ
        when (gateIsGate g) $
            mapM_ skipRec (gateArgs g)

markSave :: Monad m => Ref -> BuilderT g m ()
markSave ref = bs_circ . circ_refsave %= IS.insert (getRef ref)

markSkip :: Monad m => Ref -> BuilderT g m ()
markSkip ref = bs_circ . circ_refskip %= IS.insert (getRef ref)

markOutput :: Monad m => Ref -> BuilderT g m ()
markOutput !ref = do
    bs_circ . circ_outputs %= flip V.snoc ref
    bumpRefCount ref

markConstant :: Monad m => Int -> Ref -> BuilderT g m ()
markConstant !x !ref = bs_constants . at x ?= ref

existingConstant :: Monad m => Int -> BuilderT g m (Maybe Ref)
existingConstant !x = use (bs_constants . at x)
