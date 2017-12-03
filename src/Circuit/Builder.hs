{-# LANGUAGE DoAndIfThenElse #-}

module Circuit.Builder
  ( Builder
  , BuilderT
  , buildCircuit
  , buildCircuitT
  , setBase
  , setSymlen
  , module Circuit.Builder
  ) where

import Circuit
import Circuit.Builder.Internals
import Circuit.Utils

import Control.Monad.State
import Text.Printf
import qualified Data.Map as M
import qualified Data.Bimap as B

input :: Monad m => BuilderT m Ref
input = do
    id   <- nextInputId
    ref  <- nextRef
    insertInput ref id
    return ref

-- get the ref of a particular input, even if it does not exist already.
input_n :: Monad m => Id -> BuilderT m Ref
input_n n = do
    dedup <- gets bs_dedup
    case M.lookup (OpInput n) dedup of
        Just ref -> return ref
        Nothing  -> do
            cur <- gets bs_next_inp
            last <$> replicateM (getId n - getId cur + 1) input

inputs :: Monad m => Int -> BuilderT m [Ref]
inputs n = replicateM n input

secret :: Monad m => Integer -> BuilderT m Ref
secret val = do
    id  <- nextSecretId
    ref <- nextRef
    insertSecret ref id
    insertSecretVal id val
    return ref

-- get the ref of a particular secret, even if it does not exist already.
secret_n :: Monad m => Id -> BuilderT m Ref
secret_n n = do
    dedup <- gets bs_dedup
    case M.lookup (OpSecret n) dedup of
        Just ref -> return ref
        Nothing  -> do
            cur <- gets bs_next_secret
            last <$> replicateM (getId n - getId cur + 1) (secret 0)

secrets :: Monad m => [Integer] -> BuilderT m [Ref]
secrets = mapM secret

constant :: Monad m => Integer -> BuilderT m Ref
constant val = do
    c <- getCirc
    if B.member val (circ_consts c) then do
        return (circ_consts c B.! val)
    else do
        id  <- nextSecretId
        ref <- nextRef
        insertSecret ref id
        insertSecretVal id val
        markConst val ref id
        return ref

constants :: Monad m => [Integer] -> BuilderT m [Ref]
constants = mapM constant

circAdd :: Monad m => Ref -> Ref -> BuilderT m Ref
circAdd x y = newOp (OpAdd x y)

circSub :: Monad m => Ref -> Ref -> BuilderT m Ref
circSub x y = newOp (OpSub x y)

circMul :: Monad m => Ref -> Ref -> BuilderT m Ref
circMul x y = newOp (OpMul x y)

circProd :: Monad m => [Ref] -> BuilderT m Ref
circProd = foldTreeM circMul

circSum :: Monad m => [Ref] -> BuilderT m Ref
circSum = foldTreeM circAdd

circXor :: Monad m => Ref -> Ref -> BuilderT m Ref
circXor x y = do
    z  <- circAdd x y
    c  <- circMul x y
    c' <- circAdd c c
    circSub z c'

circXors :: Monad m => [Ref] -> BuilderT m Ref
circXors = foldTreeM circXor

circOr :: Monad m => Ref -> Ref -> BuilderT m Ref
circOr x y = do
    z <- circAdd x y
    c <- circMul x y
    circSub z c

circOrs :: Monad m => [Ref] -> BuilderT m Ref
circOrs = foldTreeM circOr

circNot :: Monad m => Ref -> BuilderT m Ref
circNot x = do
    one <- constant 1
    circSub one x

outputs :: Monad m => [Ref] -> BuilderT m ()
outputs = mapM_ markOutput

output :: Monad m => Ref -> BuilderT m ()
output = markOutput

-- NOTE: unconnected secrets from the subcircuit will be secrets in the
-- resulting composite circuit.
subcircuit' :: Monad m => Circuit -> [Ref] -> [Ref] -> BuilderT m [Ref]
subcircuit' c xs ys
    | length xs < ninputs c = error (printf "[subcircuit'] not enough inputs got %d, need %d"
                                            (length xs) (ninputs c))
    | length ys < nconsts c = error (printf "[subcircuit'] not enough consts got %d, need %d"
                                            (length ys) (nconsts c))
    | otherwise = foldCircM translate c
  where
    translate (OpAdd _ _) _ [x,y] = circAdd x y
    translate (OpSub _ _) _ [x,y] = circSub x y
    translate (OpMul _ _) _ [x,y] = circMul x y
    translate (OpInput  id) _ _ = return (xs !! getId id)
    translate (OpSecret id) _ _ = return (ys !! getId id)
    translate op _ args =
        error ("[subcircuit'] weird input: " ++ show op ++ " " ++ show args)

-- lift the subcircuit's constants and secrets into the circuit above
subcircuit :: Monad m => Circuit -> [Ref] -> BuilderT m [Ref]
subcircuit c xs = do
    ys <- exportSecrets c
    subcircuit' c xs ys

exportSecrets :: Monad m => Circuit -> BuilderT m [Ref]
exportSecrets c = do
    forM (M.toAscList (circ_secret_refs c)) $ \(_, sid) -> do
        let x = getSecret c sid
        if publicConst c sid then do
            constant x
        else
            secret x

--------------------------------------------------------------------------------
-- extras!

selectPT :: Monad m => [Ref] -> [Bool] -> BuilderT m [Ref]
selectPT xs bs = do
    one <- constant 1
    when (length xs /= length bs) $ error "[select] unequal length inputs"
    let set _   (x, True)  = return x
        set one (x, False) = circSub one x
    mapM (set one) (zip xs bs)

bitsSet :: Monad m => [Ref] -> [Bool] -> BuilderT m Ref
bitsSet xs bs = circProd =<< selectPT xs bs

-- transforms an input x into a vector [ 0 .. 1 .. 0 ] with a 1 in the xth place
selectionVector :: Monad m => [Ref] -> BuilderT m [Ref]
selectionVector xs = mapM (bitsSet xs) (permutations (length xs) [False, True])

lookupTable :: Monad m => ([Bool] -> Bool) -> [Ref] -> BuilderT m Ref
lookupTable f xs = do
    sel <- selectionVector xs
    let tt   = f <$> booleanPermutations (length xs)
        vars = snd <$> filter (\(i,_) -> tt !! i) (zip [0..] sel)
    circSum vars

lookupTableMultibit :: Monad m => ([Bool] -> [Bool]) -> [Ref] -> BuilderT m [Ref]
lookupTableMultibit f xs =
    mapM (flip lookupTable xs) [ (\x -> f x !! i) | i <- [0..noutputs - 1] ]
  where
    noutputs = length (f (replicate (length xs) False))

matrixTimesVect :: Monad m => [[Ref]] -> [Ref] -> BuilderT m [Ref]
matrixTimesVect rows vect
  | not $ all ((== length vect) . length) rows = error "[matrixTimesVect] bad dimensions"
  | otherwise = mapM (circXors <=< zipWithM circMul vect) rows

matrixTimesVectPT :: Monad m => [[Bool]] -> [Ref] -> BuilderT m [Ref]
matrixTimesVectPT rows vect
  | not $ all ((== length vect) . length) rows = error "[matrixTimesVectPT] bad dimensions"
  | otherwise = mapM (circXors <=< selectPT vect) rows

matrixMul :: Monad m => [[Ref]] -> [[Ref]] -> BuilderT m [[Ref]]
matrixMul a b = mapM (matrixTimesVect a) b
