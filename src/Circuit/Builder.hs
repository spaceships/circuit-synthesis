{-# LANGUAGE FlexibleContexts #-}

module Circuit.Builder where

import Circuit
import Util

import Control.Monad.State
import qualified Data.Map as M

type Builder = State BuildSt

data BuildSt = BuildSt {
      bs_circ        :: Circuit
    , bs_next_ref    :: Ref
    , bs_next_inp    :: Id
    , bs_next_const  :: Id
    , bs_consts      :: M.Map Id Integer
    , bs_refmap      :: M.Map String Ref
    , bs_dedup       :: M.Map Op Ref
    }

emptyBuild :: BuildSt
emptyBuild = BuildSt emptyCirc 0 0 0 M.empty M.empty M.empty

getCirc :: Builder Circuit
getCirc = gets bs_circ

modifyCirc :: (Circuit -> Circuit) -> Builder ()
modifyCirc f = modify (\st -> st { bs_circ = f (bs_circ st) })

insertOp :: Ref -> Op -> Builder ()
insertOp ref op = do
    refs <- circ_refmap <$> getCirc
    when (M.member ref refs) $
        error ("redefinition of ref " ++ show ref)
    modifyCirc (\c -> c { circ_refmap = M.insert ref op refs })
    modify (\st -> st { bs_dedup = M.insert op ref (bs_dedup st)})

insertConst :: Ref -> Id -> Builder ()
insertConst ref id = do
    refs <- circ_consts <$> getCirc
    let circ_consts' = safeInsert ("redefinition of y" ++ show id) id ref refs
    modifyCirc (\c -> c { circ_consts = circ_consts' })
    insertOp ref (OpConst id)

insertSecret :: Id -> Integer -> Builder ()
insertSecret id val = do
    ys <- circ_secrets <$> getCirc
    let ys' = safeInsert ("reassignment of y" ++ show id) id val ys
    modifyCirc (\c -> c { circ_secrets = ys' })

insertInput :: Ref -> Id -> Builder ()
insertInput ref id = do
    refs <- circ_inputs <$> getCirc
    let circ_inputs' = safeInsert ("redefinition of x" ++ show id) id ref refs
    modifyCirc (\c -> c { circ_inputs = circ_inputs' })
    insertOp ref (OpInput id)

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

nextInputId :: Builder Id
nextInputId = do
    id <- gets bs_next_inp
    modify (\st -> st { bs_next_inp = id + 1 })
    return id

nextConstId :: Builder Id
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
    id   <- nextInputId
    ref  <- nextRef
    insertInput ref id
    return ref

inputs :: Int -> Builder [Ref]
inputs n = replicateM n input

constant :: Builder Ref
constant = do
    id   <- nextConstId
    ref  <- nextRef
    insertConst ref id
    return ref

consts :: Int -> Builder [Ref]
consts n = replicateM n constant

secret :: Integer -> Builder Ref
secret val = do
    id   <- nextConstId
    ref  <- nextRef
    insertConst ref id
    insertSecret id val
    return ref

secrets :: [Integer] -> Builder [Ref]
secrets vals = mapM secret vals

circAdd :: Ref -> Ref -> Builder Ref
circAdd x y = newOp (OpAdd x y)

circSub :: Ref -> Ref -> Builder Ref
circSub x y = newOp (OpSub x y)

circMul :: Ref -> Ref -> Builder Ref
circMul x y = newOp (OpMul x y)

circSum :: [Ref] -> Builder Ref
circSum (x:xs) = foldM (\a b -> newOp (OpAdd a b)) x xs

output :: [Ref] -> Builder ()
output xs = mapM_ markOutput xs

-- NOTE: unconnected secrets from the subcircuit will be secrets in the
-- resulting composite circuit.
subcircuit :: Circuit -> [Ref] -> [Ref] -> Builder [Ref]
subcircuit c xs ys = foldCircM translate c
  where
    translate (OpAdd _ _) _ [x,y] = circAdd x y
    translate (OpSub _ _) _ [x,y] = circSub x y
    translate (OpMul _ _) _ [x,y] = circMul x y
    translate (OpInput id) _ _ = if (getId id >= length xs) then input else return (xs !! getId id)
    translate (OpConst id) _ _ = if (getId id >= length ys)
                                    then if M.member id (circ_secrets c)
                                            then secret (circ_secrets c M.! id)
                                            else constant
                                    else return (ys !! getId id)
    eval op ref args = error ("[subCircuit] weird input: " ++ show op ++ " " ++ show args)

subcircuit' :: Circuit -> [Ref] -> Builder [Ref]
subcircuit' c xs = subcircuit c xs []
