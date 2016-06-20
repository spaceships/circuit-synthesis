{-# LANGUAGE FlexibleContexts #-}

module Circuit.Builder where

import Circuit

import Control.Monad.State
import qualified Data.Map as M

type Builder = State BuildSt

data BuildSt = BuildSt {
      bs_circ        :: Circuit
    , bs_next_ref    :: Ref
    , bs_next_inp    :: ID
    , bs_next_const  :: ID
    , bs_consts      :: M.Map ID Integer
    , bs_refmap      :: M.Map String Ref
    , bs_dedup       :: M.Map Op Ref
    }

emptyBuild :: BuildSt
emptyBuild = BuildSt emptyCirc 0 0 0 M.empty M.empty M.empty

getCirc :: MonadState BuildSt m => m Circuit
getCirc = bs_circ <$> get

modifyCirc :: MonadState BuildSt m => (Circuit -> Circuit) -> m ()
modifyCirc f = modify (\st -> st { bs_circ = f (bs_circ st) })

insertConst :: MonadState BuildSt m => ID -> Integer -> m ()
insertConst i c = modify (\st -> st { bs_consts = M.insert i c (bs_consts st)})

insertOp :: MonadState BuildSt m => Ref -> Op -> m ()
insertOp ref op = do
    refs <- circ_refmap <$> getCirc
    when (M.member ref refs) $
        error ("redefinition of ref " ++ show ref)
    modifyCirc (\c -> c { circ_refmap = M.insert ref op refs })
    modify (\st -> st { bs_dedup = M.insert op ref (bs_dedup st)})

newOp :: Op -> Builder Ref
newOp op = do
    dedup <- gets bs_dedup
    case M.lookup op dedup of
        Nothing -> do
            ref <- nextRef
            insertOp ref op
            return ref
        Just ref ->
            return ref

nextRef :: Builder Ref
nextRef = do
    ref <- gets bs_next_ref
    modify (\st -> st { bs_next_ref = ref + 1 })
    return ref

nextInputId :: Builder ID
nextInputId = do
    id <- gets bs_next_inp
    modify (\st -> st { bs_next_inp = id + 1 })
    return id

nextConstId :: Builder ID
nextConstId = do
    id <- gets bs_next_const
    modify (\st -> st { bs_next_const = id + 1 })
    return id

markOutput :: Ref -> Builder ()
markOutput ref = modifyCirc (\c -> c { circ_outrefs = circ_outrefs c ++ [ref] })

--------------------------------------------------------------------------------
-- smart constructors

buildCircuit :: Builder a -> Circuit
buildCircuit = bs_circ . flip execState emptyBuild

input :: Builder Ref
input = do
    id  <- nextInputId
    newOp (Input id)

inputs :: Int -> Builder [Ref]
inputs n = replicateM n input

secret :: Builder Ref
secret = do
    id  <- nextConstId
    newOp (Const id)

secrets :: Int -> Builder [Ref]
secrets n = replicateM n secret

circAdd :: Ref -> Ref -> Builder Ref
circAdd x y = newOp (Add x y)

circSub :: Ref -> Ref -> Builder Ref
circSub x y = newOp (Sub x y)

circMul :: Ref -> Ref -> Builder Ref
circMul x y = newOp (Mul x y)

circSum :: [Ref] -> Builder Ref
circSum (x:xs) = foldM (\a b -> newOp (Add a b)) x xs

output :: [Ref] -> Builder ()
output xs = mapM_ markOutput xs

subcircuit :: Circuit -> [Ref] -> [Ref] -> Builder [Ref]
subcircuit c xs ys = foldCircM translate c
  where
    translate (Input id) _ _ = if (id >= length xs) then input  else return (xs!!id)
    translate (Const id) _ _ = if (id >= length ys) then secret else return (ys!!id)
    translate (Add _ _) _ [x,y] = circAdd x y
    translate (Sub _ _) _ [x,y] = circSub x y
    translate (Mul _ _) _ [x,y] = circMul x y
    eval op ref args = error ("[subCircuit] weird input: " ++ show op ++ " " ++ show args)

subcircuit' :: Circuit -> [Ref] -> Builder [Ref]
subcircuit' c xs = subcircuit c xs []
