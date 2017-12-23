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

import Control.Monad
import Lens.Micro.Platform
import Text.Printf
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

input :: (Gate g, Monad m) => BuilderT g m Ref
input = do
    id   <- nextInputId
    ref  <- nextRef
    insertInput ref id
    return ref

-- get the ref of a particular input, even if it does not exist already.
input_n :: (Gate g, Monad m) => Id -> BuilderT g m Ref
input_n n = do
    dedup <- use bs_dedup
    case M.lookup (gateInput n) dedup of
        Just ref -> return ref
        Nothing  -> do
            cur <- use bs_next_inp
            last <$> replicateM (getId n - getId cur + 1) input

inputs :: (Gate g, Monad m) => Int -> BuilderT g m [Ref]
inputs n = replicateM n input

secret :: (Gate g, Monad m) => Integer -> BuilderT g m Ref
secret val = do
    id  <- nextConstId
    ref <- nextRef
    insertConst ref id
    insertConstVal id val
    markSecret ref
    return ref

-- get the ref of a particular secret, even if it does not exist already (inserts secret 0s)
secret_n :: (Gate g, Monad m) => Id -> BuilderT g m Ref
secret_n n = do
    dedup <- use bs_dedup
    case M.lookup (gateConst n) dedup of
        Just ref -> return ref
        Nothing  -> do
            cur <- use bs_next_const
            last <$> replicateM (getId n - getId cur + 1) (secret 0)

-- preserve duplication: there will be many gates for secret 0
secrets :: (Gate g, Monad m) => [Integer] -> BuilderT g m [Ref]
secrets = mapM secret

-- avoid duplication: there will be only one gate for const 0
constant :: (Gate g, Monad m) => Integer -> BuilderT g m Ref
constant val = do
    r <- existingConstant val
    case r of
        Just ref -> return ref
        Nothing  -> do
            id  <- nextConstId
            ref <- nextRef
            insertConst ref id
            insertConstVal id val
            markConstant val ref
            return ref

constants :: (Gate g, Monad m) => [Integer] -> BuilderT g m [Ref]
constants = mapM constant

circAdd :: (Gate g, Monad m) => Ref -> Ref -> BuilderT g m Ref
circAdd x y = newGate (gateAdd x y)

circSub :: (Gate g, Monad m) => Ref -> Ref -> BuilderT g m Ref
circSub x y = newGate (gateSub x y)

circMul :: (Gate g, Monad m) => Ref -> Ref -> BuilderT g m Ref
circMul x y = newGate (gateMul x y)

circProd :: (Gate g, Monad m) => [Ref] -> BuilderT g m Ref
circProd = foldTreeM circMul

circSum :: (Gate g, Monad m) => [Ref] -> BuilderT g m Ref
circSum = foldTreeM circAdd

circXor :: (Gate g, Monad m) => Ref -> Ref -> BuilderT g m Ref
circXor x y = case gateXor x y of
    Just g  -> newGate g
    Nothing -> do
        z  <- circAdd x y
        c  <- circMul x y
        c' <- circAdd c c
        circSub z c'

circXors :: (Gate g, Monad m) => [Ref] -> BuilderT g m Ref
circXors = foldTreeM circXor

circOr :: (Gate g, Monad m) => Ref -> Ref -> BuilderT g m Ref
circOr x y = do
    z <- circAdd x y
    c <- circMul x y
    circSub z c

circOrs :: (Gate g, Monad m) => [Ref] -> BuilderT g m Ref
circOrs = foldTreeM circOr

circNot :: (Gate g, Monad m) => Ref -> BuilderT g m Ref
circNot x = case gateNot x of
    Just g  -> newGate g
    Nothing -> do
        one <- constant 1
        circSub one x

outputs :: Monad m => [Ref] -> BuilderT g m ()
outputs = mapM_ markOutput

output :: Monad m => Ref -> BuilderT g m ()
output = markOutput

-- NOTE: unconnected secrets from the subcircuit will be secrets in the
-- resulting composite circuit.
subcircuit' :: Monad m => Circuit ArithGate -> [Ref] -> [Ref] -> BuilderT ArithGate m [Ref]
subcircuit' c xs ys
    | length xs < ninputs c = error (printf "[subcircuit'] not enough inputs got %d, need %d"
                                            (length xs) (ninputs c))
    | length ys < nconsts c = error (printf "[subcircuit'] not enough consts got %d, need %d"
                                            (length ys) (nconsts c))
    | otherwise = foldCircM translate c
  where
    translate (ArithAdd _ _) _ [x,y] = circAdd x y
    translate (ArithSub _ _) _ [x,y] = circSub x y
    translate (ArithMul _ _) _ [x,y] = circMul x y
    translate (ArithInput id) _ _ = return (xs !! getId id)
    translate (ArithConst id) _ _ = return (ys !! getId id)
    translate op _ args =
        error ("[subcircuit'] weird input: " ++ show op ++ " " ++ show args)

-- lift the subcircuit's constants and secrets into the circuit above
subcircuit :: Monad m => Circuit ArithGate -> [Ref] -> BuilderT ArithGate m [Ref]
subcircuit c xs = do
    ys <- exportConsts c
    subcircuit' c xs ys

exportConsts :: (Gate g, Monad m) => Circuit g -> BuilderT g m [Ref]
exportConsts c = do
    forM (IM.toAscList (c^.circ_consts)) $ \(_, id) -> do
        let x = getConst c id
        if secretConst c id then do
            secret x
        else
            constant x

exportParams :: Monad m => Circuit g -> BuilderT g m ()
exportParams c = do
    setSymlen (c^.circ_symlen)
    setBase (c^.circ_base)

--------------------------------------------------------------------------------
-- extras!

-- select the ix'th bit from x
select :: Monad m => [Ref] -> [Ref] -> BuilderT ArithGate m Ref
select xs ix = do
    sel <- selectionVector ix
    zs  <- zipWithM (circMul) sel xs
    circSum zs

selects :: Monad m => [Ref] -> [[Ref]] -> BuilderT ArithGate m [Ref]
selects xs ixs = mapM (select xs) ixs

selectsPT :: (Gate g, Monad m) => [Int] -> [Ref] -> BuilderT g m [Ref]
selectsPT sels xs = return (map (xs!!) sels)

selectPT :: (Gate g, Monad m) => [Ref] -> [Bool] -> BuilderT g m [Ref]
selectPT xs bs = do
    one <- constant 1
    when (length xs /= length bs) $ error "[select] unequal length inputs"
    let set _   (x, True)  = return x
        set one (x, False) = circSub one x
    mapM (set one) (zip xs bs)

bitsSet :: (Gate g, Monad m) => [Ref] -> [Bool] -> BuilderT g m Ref
bitsSet xs bs = circProd =<< selectPT xs bs

-- transforms an input x into a vector [ 0 .. 1 .. 0 ] with a 1 in the xth place
selectionVector :: (Gate g, Monad m) => [Ref] -> BuilderT g m [Ref]
selectionVector xs = mapM (bitsSet xs) (permutations (length xs) [False, True])

lookupTable :: (Gate g, Monad m) => ([Bool] -> Bool) -> [Ref] -> BuilderT g m Ref
lookupTable f xs = do
    sel <- selectionVector xs
    let tt   = f <$> booleanPermutations (length xs)
        vars = snd <$> filter (\(i,_) -> tt !! i) (zip [0..] sel)
    circSum vars

lookupTableMultibit :: (Gate g, Monad m) => ([Bool] -> [Bool]) -> [Ref] -> BuilderT g m [Ref]
lookupTableMultibit f xs =
    mapM (flip lookupTable xs) [ (\x -> f x !! i) | i <- [0..noutputs - 1] ]
  where
    noutputs = length (f (replicate (length xs) False))

matrixTimesVect :: (Gate g, Monad m) => [[Ref]] -> [Ref] -> BuilderT g m [Ref]
matrixTimesVect rows vect
  | not $ all ((== length vect) . length) rows = error "[matrixTimesVect] bad dimensions"
  | otherwise = mapM (circXors <=< zipWithM circMul vect) rows

matrixTimesVectPT :: (Gate g, Monad m) => [[Bool]] -> [Ref] -> BuilderT g m [Ref]
matrixTimesVectPT rows vect
  | not $ all ((== length vect) . length) rows = error "[matrixTimesVectPT] bad dimensions"
  | otherwise = mapM (circXors <=< selectPT vect) rows

matrixMul :: (Gate g, Monad m) => [[Ref]] -> [[Ref]] -> BuilderT g m [[Ref]]
matrixMul a b = mapM (matrixTimesVect a) b

-- swap elements based on a bit in the circuit
swap :: (Gate g, Monad m) => Ref -> [Ref] -> [Ref] -> BuilderT g m [[Ref]]
swap b xs ys
 | length xs /= length ys = error "[swap] unequal length inputs!"
 | otherwise = do
    not_b <- circNot b
    let n = length xs
        sel = replicate n b
        not_sel = replicate n not_b
    w0 <- zipWithM circMul not_sel xs
    w1 <- zipWithM circMul sel ys
    w2 <- zipWithM circAdd w0 w1

    z0 <- zipWithM circMul not_sel ys
    z1 <- zipWithM circMul sel xs
    z2 <- zipWithM circAdd z0 z1

    return [w2,z2]
