module Circuit.Builder
  ( Builder
  , BuilderT
  , buildCircuit
  , buildCircuitT
  , setBase
  , setSymlen
  , setSigma
  , Circuit.Builder.Internals.markPersistant
  , module Circuit.Builder
  ) where

import Circuit
import Circuit.Builder.Internals
import Circuit.Utils

import Control.Monad
import Data.Array
import Lens.Micro.Platform
import Text.Printf
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM

-- inputBit does not create a new symbol: useful as a primitive
inputBit :: (Gate g, Monad m) => BuilderT g m Ref
inputBit = do
    id   <- nextInputId
    ref  <- nextRef
    insertInput ref id
    return ref

inputBits :: (Gate g, Monad m) => Int -> BuilderT g m [Ref]
inputBits n = replicateM n inputBit

-- create a length 1 input symbol
input :: (Gate g, Monad m) => BuilderT g m Ref
input = head <$> symbol 1

-- create n length 1 input symbols
inputs :: (Gate g, Monad m) => Int -> BuilderT g m [Ref]
inputs n = replicateM n input

-- get the ref of a particular input, even if it does not exist already.
-- assumes length 1 symbols.
input_n :: (Gate g, Monad m) => Id -> BuilderT g m Ref
input_n n = do
    dedup <- use bs_dedup
    case M.lookup (gateBase (Input n)) dedup of
        Just ref -> return ref
        Nothing  -> do
            cur <- use bs_next_inp
            last <$> replicateM (getId n - getId cur + 1) input

symbol :: (Gate g, Monad m) => Int -> BuilderT g m [Ref]
symbol len = do
    refs <- inputBits len
    i <- nextSymbol
    setSymlen i len
    return refs

sigma :: (Gate g, Monad m) => Int -> BuilderT g m [Ref]
sigma len = do
    refs <- inputBits len
    i <- nextSymbol
    setSymlen i len
    setSigma i
    return refs

secret :: (Gate g, Monad m) => Int -> BuilderT g m Ref
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
    case M.lookup (gateBase (Const n)) dedup of
        Just ref -> return ref
        Nothing  -> do
            cur <- use bs_next_const
            last <$> replicateM (getId n - getId cur + 1) (secret 0)

-- preserve duplication: there will be many gates for secret 0
secrets :: (Gate g, Monad m) => [Int] -> BuilderT g m [Ref]
secrets = mapM secret

-- avoid duplication: there will be only one gate for const 0
constant :: (Gate g, Monad m) => Int -> BuilderT g m Ref
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

constants :: (Gate g, Monad m) => [Int] -> BuilderT g m [Ref]
constants = mapM constant

circAdd :: (Gate g, Monad m) => Ref -> Ref -> BuilderT g m Ref
circAdd x y = newGate (gateAdd x y)

circSub :: (Gate g, Monad m) => Ref -> Ref -> BuilderT g m Ref
circSub x y = newGate (gateSub x y)

circMul :: (Gate g, Monad m) => Ref -> Ref -> BuilderT g m Ref
circMul x y = newGate (gateMul x y)

circAnd :: (Gate g, Monad m) => Ref -> Ref -> BuilderT g m Ref
circAnd = circMul

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
subcircuit' :: (Monad m, Gate g) => Circuit g -> [Ref] -> [Ref] -> BuilderT g m [Ref]
subcircuit' c xs ys
    | length xs < ninputs c = error (printf "[subcircuit'] not enough inputs got %d, need %d"
                                            (length xs) (ninputs c))
    | length ys < nconsts c = error (printf "[subcircuit'] not enough consts got %d, need %d"
                                            (length ys) (nconsts c))
    | otherwise = foldCircM translate c
  where
    xs' = listArray (0, length xs-1) xs
    ys' = listArray (0, length ys-1) ys

    translate g _ args = if gateIsGate g
                            then newGate (gateFix g args)
                            else case gateGetBase g of
                                Just (Input id) -> return (xs' ! getId id)
                                Just (Const id) -> return (ys' ! getId id)
                                Nothing -> error "[subcircuit'] this should never happen"

-- lift the subcircuit's constants and secrets into the circuit above
subcircuit :: (Gate g, Monad m) => Circuit g -> [Ref] -> BuilderT g m [Ref]
subcircuit c xs = do
    ys <- exportConsts c
    subcircuit' c xs ys

exportConsts :: (Gate g, Gate g', Monad m) => Circuit g -> BuilderT g' m [Ref]
exportConsts c = do
    forM (IM.toAscList (c^.circ_consts)) $ \(_, id) -> do
        let x = getConst c id
        if secretConst c id then do
            secret x
        else
            constant x

exportParams :: (Gate g, Gate g', Monad m) => Circuit g -> BuilderT g' m ()
exportParams c = do
    setBase (c^.circ_base)
    mapM_ (uncurry setSymlen) (IM.toList (c^.circ_symlen))

--------------------------------------------------------------------------------
-- extras!

-- select the ix'th bit from x
select :: (Gate g, Monad m) => [Ref] -> [Ref] -> BuilderT g m Ref
select xs ix = do
    sel <- selectionVector ix
    selectSigma xs ix

selectSigma :: (Gate g, Monad m) => [Ref] -> [Ref] -> BuilderT g m Ref
selectSigma xs sel = circSum =<< zipWithM (circMul) sel xs

selects :: (Gate g, Monad m) => [Ref] -> [[Ref]] -> BuilderT g m [Ref]
selects xs ixs = mapM (select xs) ixs

-- select the ith list from a list of lists, ix is binary
selectList :: (Gate g, Monad m) => [Ref] -> [[Ref]] -> BuilderT g m [Ref]
selectList ix xs = do
    sel <- selectionVector ix
    selectListSigma sel xs

-- select the ith list from a list of lists, ix is sigma vector
selectListSigma :: (Gate g, Monad m) => [Ref] -> [[Ref]] -> BuilderT g m [Ref]
selectListSigma ix xs = do
    let sels = map (replicate (length (head xs))) ix
    masked <- zipWithM (zipWithM circMul) sels xs
    foldM1 (zipWithM circAdd) masked

selectsPT :: [a] -> [Int] -> [a]
selectsPT xs sels = map look sels
  where
    xs' = listArray (0, length xs - 1) xs
    look i = let res = xs' ! i in res `seq` res

selectPT :: (Gate g, Monad m) => [Ref] -> [Bool] -> BuilderT g m [Ref]
selectPT xs bs = do
    one <- constant 1
    when (length xs /= length bs) $ error "[select] unequal length inputs"
    let set _   (x, True)  = return x
        set one (x, False) = circSub one x
    mapM (set one) (zip xs bs)

bitsSet :: (Gate g, Monad m) => [Ref] -> [Bool] -> BuilderT g m Ref
bitsSet xs bs = circProd =<< selectPT xs bs

-- Transform a binary input x into a vector [ 0 .. 1 .. 0 ] with a 1 in the xth place
selectionVector :: (Gate g, Monad m) => [Ref] -> BuilderT g m [Ref]
selectionVector xs = mapM (bitsSet xs) (permutations (length xs) [False, True])

-- produces a selection vector for i with length len
selectionVectorInt :: (Gate g, Monad m) => Int -> Int -> BuilderT g m [Ref]
selectionVectorInt i len
  | i >= len = error "[selectInt] i >= len!"
  | otherwise = do
    one  <- constant 1
    zero <- constant 0
    if i == 0 then
        return $ one : replicate (len-1) zero
    else
        return $ replicate (i-1) zero ++ [one] ++ replicate (len - i) zero

-- Combine sigma vectors. First sigma vector is most significant.
-- this builds a formula that says "[x0 & y0, x0 & y1, ... , xn & yn-1, xn & yn]"
sigmaProd :: (Gate g, Monad m) => [[Ref]] -> BuilderT g m [Ref]
sigmaProd sels = mapM circProd (sequence sels)

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
