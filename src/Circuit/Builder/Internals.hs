{-# LANGUAGE BangPatterns #-}

module Circuit.Builder.Internals where

import Circuit
import Circuit.Utils

import Control.Monad.State.Strict
import Control.Monad.Identity
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as S

type BuilderT = StateT BuildSt
type Builder = BuilderT Identity

data BuildSt = BuildSt {
      bs_circ        :: !Circuit
    , bs_next_ref    :: !Ref
    , bs_next_inp    :: !Id
    , bs_next_secret :: !Id
    , bs_dedup       :: !(M.Map Op Ref)
    }

emptyBuild :: BuildSt
emptyBuild = BuildSt emptyCirc 0 0 0 M.empty

runCircuitT :: Monad m => BuilderT m a -> m (Circuit, a)
runCircuitT b = do
    (a, st) <- runStateT b emptyBuild
    return (bs_circ st, a)

buildCircuitT :: Monad m => BuilderT m a -> m Circuit
buildCircuitT b = bs_circ <$> execStateT b emptyBuild

runCircuit :: Builder a -> (Circuit, a)
runCircuit b = runIdentity (runCircuitT b)

buildCircuit :: Builder a -> Circuit
buildCircuit = bs_circ . flip execState emptyBuild

--------------------------------------------------------------------------------
-- operations

getCirc :: Monad m => BuilderT m Circuit
getCirc = gets bs_circ

modifyCirc :: Monad m => (Circuit -> Circuit) -> BuilderT m ()
modifyCirc f = modify' (\st -> st { bs_circ = f (bs_circ st) })

exportParams :: Monad m => Circuit -> BuilderT m ()
exportParams c = do
    setSymlen (circ_symlen c)
    setBase (circ_base c)

setSymlen :: Monad m => Int -> BuilderT m ()
setSymlen n = modifyCirc (\c -> c { circ_symlen = n })

setBase :: Monad m => Int -> BuilderT m ()
setBase n = modifyCirc (\c -> c { circ_base = n })

insertOp :: Monad m => Ref -> Op -> BuilderT m ()
insertOp !ref !op = do
    refs <- circ_refmap <$> getCirc
    when (IM.member (getRef ref) refs) $
        error ("redefinition of ref " ++ show ref)
    modifyCirc (\c -> c { circ_refmap = IM.insert (getRef ref) op refs })
    modify' (\st -> st { bs_dedup = M.insert op ref (bs_dedup st)})

insertSecret :: Monad m => Ref -> Id -> BuilderT m ()
insertSecret !ref !id = do
    modifyCirc (\c -> c { circ_secret_refs = M.insert ref id (circ_secret_refs c) })
    insertOp ref (OpSecret id)

insertSecretVal :: Monad m => Id -> Integer -> BuilderT m ()
insertSecretVal id val = do
    ys <- circ_secrets <$> getCirc
    let ys' = safeInsert ("reassignment of y" ++ show id) id val ys
    modifyCirc (\c -> c { circ_secrets = ys' })

insertInput :: Monad m => Ref -> Id -> BuilderT m ()
insertInput ref id = do
    modifyCirc (\c -> c { circ_inputs = circ_inputs c ++ [ref] })
    insertOp ref (OpInput id)

newOp :: Monad m => Op -> BuilderT m Ref
newOp op = do
    dedup <- gets bs_dedup
    case M.lookup op dedup of
        Nothing -> do
            ref <- nextRef
            insertOp ref op
            return ref
        Just ref -> do
            return ref

nextRef :: Monad m => BuilderT m Ref
nextRef = do
    ref <- gets bs_next_ref
    modify' (\st -> st { bs_next_ref = ref + 1 })
    return ref

nextInputId :: Monad m => BuilderT m Id
nextInputId = do
    id <- gets bs_next_inp
    modify' (\st -> st { bs_next_inp = id + 1 })
    return id

nextSecretId :: Monad m => BuilderT m Id
nextSecretId = do
    id <- gets bs_next_secret
    modify' (\st -> st { bs_next_secret = id + 1 })
    return id

markOutput :: Monad m => Ref -> BuilderT m ()
markOutput ref = modifyCirc (\c -> c { circ_outputs = circ_outputs c ++ [ref] })

markConst :: Monad m => Integer -> Ref -> Id -> BuilderT m ()
markConst val ref id = modifyCirc (\c -> c { circ_consts    = M.insert val ref (circ_consts c)
                                           , circ_const_ids = S.insert (getId id) (circ_const_ids c)
                                           })
