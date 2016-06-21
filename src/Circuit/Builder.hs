{-# LANGUAGE FlexibleContexts #-}

module Circuit.Builder where

import Circuit
import Util

import Control.Monad.State
import Text.Printf
import qualified Data.Map as M

type Builder = State BuildSt

data BuildSt = BuildSt {
      bs_circ        :: Circuit
    , bs_next_ref    :: Ref
    , bs_next_inp    :: Id
    , bs_next_const  :: Id
    , bs_refmap      :: M.Map String Ref
    , bs_dedup       :: M.Map Op Ref
    }

emptyBuild :: BuildSt
emptyBuild = BuildSt emptyCirc 0 0 0 M.empty M.empty

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
    modifyCirc (\c -> c { circ_consts = circ_consts c ++ [ref] })
    insertOp ref (OpConst id)

insertSecret :: Id -> Integer -> Builder ()
insertSecret id val = do
    ys <- circ_secrets <$> getCirc
    let ys' = safeInsert ("reassignment of y" ++ show id) id val ys
    modifyCirc (\c -> c { circ_secrets = ys' })

insertInput :: Ref -> Id -> Builder ()
insertInput ref id = do
    modifyCirc (\c -> c { circ_inputs = circ_inputs c ++ [ref] })
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
markOutput ref = modifyCirc (\c -> c { circ_outputs = circ_outputs c ++ [ref] })

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
secrets = mapM secret

circAdd :: Ref -> Ref -> Builder Ref
circAdd x y = newOp (OpAdd x y)

circSub :: Ref -> Ref -> Builder Ref
circSub x y = newOp (OpSub x y)

circMul :: Ref -> Ref -> Builder Ref
circMul x y = newOp (OpMul x y)

circSum :: [Ref] -> Builder Ref
circSum (x:xs) = foldM (\a b -> newOp (OpAdd a b)) x xs

output :: [Ref] -> Builder ()
output = mapM_ markOutput

-- NOTE: unconnected secrets from the subcircuit will be secrets in the
-- resulting composite circuit.
subcircuit :: Circuit -> [Ref] -> [Ref] -> Builder [Ref]
subcircuit c xs ys
    | length xs < ninputs c = error (printf "[subcircuit] not enough inputs got %d, need %d"
                                            (length xs) (ninputs c))
    | length ys < nconsts c = error (printf "[subcircuit] not enough consts got %d, need %d"
                                            (length ys) (nconsts c))
    | otherwise = foldCircM translate c
  where
    translate (OpAdd _ _) _ [x,y] = circAdd x y
    translate (OpSub _ _) _ [x,y] = circSub x y
    translate (OpMul _ _) _ [x,y] = circMul x y
    translate (OpInput id) _ _ = return (xs !! getId id)
    translate (OpConst id) _ _ = return (ys !! getId id)
    eval op ref args = error ("[subCircuit] weird input: " ++ show op ++ " " ++ show args)

-- lift the subcircuit's secrets into the circuit
subcircuit' :: Circuit -> [Ref] -> Builder [Ref]
subcircuit' c xs = do
    ys <- secrets $ map snd (M.toAscList (circ_secrets c))
    subcircuit c xs ys
