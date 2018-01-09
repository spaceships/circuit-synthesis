{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE Strict #-}
#endif

module Circuit
    ( module Circuit.Types
    , module Circuit
    ) where

import Circuit.Types
import Circuit.Utils

import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.List (find,nub)
import Lens.Micro.Platform
import Text.Printf
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V

--------------------------------------------------------------------------------
-- Generic circuit functions

emptyCirc :: Circuit a
emptyCirc = Circuit
    { _circ_outputs     = []
    , _circ_inputs      = IS.empty
    , _circ_consts      = IM.empty
    , _circ_secret_refs = IS.empty
    , _circ_refmap      = IM.empty
    , _circ_const_vals  = IM.empty
    , _circ_symlen      = 1
    , _circ_base        = 2
    , _circ_refcount    = IM.empty
    , _circ_maxref      = 0
    , _circ_sigma_vecs  = IS.empty
    }

nwires :: Circuit gate -> Int
nwires = IM.size . _circ_refmap

ngates :: Circuit gate -> Int
ngates c = nwires c - ninputs c - nconsts c

ninputs :: Circuit gate -> Int
ninputs = IS.size . _circ_inputs

nconsts :: Circuit gate -> Int
nconsts = length . _circ_consts

nsecrets :: Circuit gate -> Int
nsecrets = IS.size . _circ_secret_refs

noutputs :: Circuit gate -> Int
noutputs = length . _circ_outputs

symlen :: Circuit gate -> Int
symlen = _circ_symlen

wires :: Gate gate => Circuit gate -> [(Ref, gate)]
wires c = map (\ref -> (ref, getGate c ref)) (wireRefs c)

wireRefs :: Gate gate => Circuit gate -> [Ref]
wireRefs = map Ref . IM.keys . view circ_refmap

-- only non-input or non-const gates
gates :: Gate gate => Circuit gate -> [(Ref, gate)]
gates = filter (gateIsGate.snd) . wires

gateRefs :: Gate gate => Circuit gate -> [Ref]
gateRefs = map fst . gates

outputRefs :: Gate gate => Circuit gate -> [Ref]
outputRefs c = V.toList (c^.circ_outputs)

inputRefs :: Gate gate => Circuit gate -> [Ref]
inputRefs c = map Ref $ IS.toList (c^.circ_inputs)

constRefs :: Gate gate => Circuit gate -> [Ref]
constRefs c = map Ref $ IM.keys (c^.circ_consts)

isOutputRef :: Gate gate => Circuit gate -> Ref -> Bool
isOutputRef c ref = V.elem ref (c^.circ_outputs)

intermediateGates :: Gate gate => Circuit gate -> [(Ref, gate)]
intermediateGates c = filter intermediate (gates c)
  where
    intermediate (ref,_) = not (isOutputRef c ref)

intermediateGateRefs :: Gate gate => Circuit gate -> [Ref]
intermediateGateRefs = map fst . intermediateGates

intermediateWireRefs :: Gate gate => Circuit gate -> [Ref]
intermediateWireRefs c = filter (not . isOutputRef c) (wireRefs c)

getConst :: Circuit gate -> Id -> Int
getConst c id = case c^.circ_const_vals.at (getId id) of
    Just x  -> x
    Nothing -> error ("[getConst] no const known for y" ++ show id)

secretConst :: Circuit gate -> Id -> Bool
secretConst c id = case find ((==id) . snd) (IM.toList (c^.circ_consts)) of
    Nothing    -> False
    Just (r,_) -> IS.member r (c^.circ_secret_refs)

secretRefs :: Circuit gate -> [Ref]
secretRefs c = map Ref $ IS.toAscList (_circ_secret_refs c)

secretIds :: Circuit gate -> [Id]
secretIds c = map (((IM.!) (c^.circ_consts)) . getRef) (secretRefs c)

getGate :: Circuit gate -> Ref -> gate
getGate c ref = case c^.circ_refmap.at (getRef ref) of
    Nothing   -> error (printf "[getGate] no ref %d!" (getRef ref))
    Just gate -> gate

randomizeSecrets :: Circuit gate -> IO (Circuit gate)
randomizeSecrets c = do
    key <- replicateM (nsecrets c) $ randIntModIO (_circ_base c)
    let newSecrets = IM.fromList $ zip (map getId (secretIds c)) key
    return $ c & circ_const_vals %~ IM.union newSecrets

genTest :: Gate gate => Circuit gate -> IO TestCase
genTest c
    | c^.circ_symlen == 1 = do
        let q = fromIntegral $ _circ_base c ^ ninputs c
        inp <- num2Base (fromIntegral (_circ_base c)) (ninputs c) <$> randIntegerModIO q
        return (inp, plainEval c inp)
    | otherwise = do
        -- symlen > 1: this only changes what we do if a particular symbol is marked as a sigma vector

        -- check that symlen is reasonable given this many inputs
        when ((ninputs c `mod` view circ_symlen c) /= 0) $
            error ("[genTest] inputs not evenly dividable by symlen " ++ show (c^.circ_symlen))

        let nsyms = ninputs c `div` _circ_symlen c

        inp <- fmap concat $ randIO $ forM ([0..nsyms-1] :: [Int]) $ \i -> do
            if IS.member i (c^.circ_sigma_vecs) then do
                let q = c^.circ_symlen
                x <- randIntMod q
                return [ if j == x then 1 else 0 | j <- [0..q-1] ]
            else do
                let b = fromIntegral (c^.circ_base)
                    p = fromIntegral (c^.circ_symlen)
                num2Base b p <$> randIntegerMod (b^p)

        return (inp, plainEval c inp)


printCircInfo :: Gate g => Circuit g -> IO ()
printCircInfo c = do
    let n = ninputs c
    printf "circuit info\n"
    printf "============\n"
    printf "ninputs=%d noutputs=%d nconsts=%d nsecrets=%d\n" n (noutputs c) (nconsts c) (nsecrets c)
    printf "symlen=%d base=%d\n" (symlen c) (c^.circ_base)
    printf "nwires=%d depth=%d\n" (nwires c) (depth c)
    printf "degree=%d\n" (circDegree c)

printTruthTable :: Gate gate => Circuit gate -> IO ()
printTruthTable c = forM_ inputs $ \inp -> do
    let out = plainEval c inp
    printf "%s -> %s\n" (showInts inp) (showInts out)
  where
    n = ninputs c `div` symlen c
    sym x = [ if i == x then 1 else 0 | i <- [ 0 .. symlen c - 1 ] ]
    inputs = case symlen c of
        1 -> sequence (replicate (ninputs c) [0..c^.circ_base - 1])
        _ -> map concat $ sequence (replicate n (map sym [0..symlen c - 1]))

circEq :: Gate gate => Circuit gate -> Circuit gate -> IO Bool
circEq c0 c1
  | ninputs  c0 /= ninputs  c1 = return False
  | noutputs c0 /= noutputs c1 = return False
  | otherwise = do
    let n = 16
    t0 <- replicateM n (genTest c0)
    t1 <- replicateM n (genTest c1)
    x  <- ensure False c1 t0
    y  <- ensure False c0 t1
    return (x && y)

ydeg :: Circuit ArithGate -> Integer
ydeg c = head (degs c)

xdeg :: Circuit ArithGate -> Int -> Integer
xdeg c i = degs c !! (i+1)

degs :: Circuit ArithGate -> [Integer]
degs c = map (varDegree c) ids
  where
    ids = ArithConst (Id (-1)) : map (ArithInput . Id) [0 .. ninputs c-1]

depth :: Gate gate => Circuit gate -> Integer
depth c = maximum $ foldCirc f c
  where
    f _ args = if null args then 0 else maximum args + 1

varDegree :: Circuit ArithGate -> ArithGate -> Integer
varDegree c z = maximum (varDegree' c z)

varDegree' :: Circuit ArithGate -> ArithGate -> [Integer]
varDegree' c z = foldCirc f c
  where
    f (ArithAdd _ _) [x,y] = max x y
    f (ArithSub _ _) [x,y] = max x y
    f (ArithMul _ _) [x,y] = x + y

    f x _ = if eq x z then 1 else 0

    eq (ArithInput x) (ArithInput y) = x == y
    eq (ArithConst _) (ArithConst _) = True
    eq _ _ = False

circDegree :: Gate g => Circuit g -> Integer
circDegree c = maximum $ foldCirc f c
  where
    f g [x,y] = if gateIsMul g then x + y else max x y
    f g [x] = x
    f _ [] = 1 -- input or const

zeroTest :: Int -> Int
zeroTest 0 = 0
zeroTest _ = 1

plainEval :: Gate gate => Circuit gate -> [Int] -> [Int]
plainEval c inps
    | ninputs c /= length inps =
        error (printf "[plainEval] incorrect number of inputs: expected %d, got %s" (ninputs c) (show inps))
    | otherwise = map zeroTest $ foldCirc (gateEval getInp (getConst c)) c
  where
    getInp i = inps !! getId i

ensure :: Gate gate => Bool -> Circuit gate -> [TestCase] -> IO Bool
ensure verbose c ts = and <$> mapM ensure' (zip [0::Int ..] ts)
  where
    ensure' (i, (inps, outs)) = do
        let res = plainEval c inps
        if res == outs then do
            let s = printf "test %d succeeded: input:%s expected:%s got:%s"
                            i (showInts inps) (showInts outs) (showInts res)
            when verbose (putStrLn s)
            return True
        else do
            let s = printf "test %d failed! input:%s expected:%s got:%s"
                            i (showInts inps) (showInts outs) (showInts res)
            putStrLn (red s)
            return False


ensureIO :: Bool -> (Circuit gate -> [Int] -> IO [Int]) -> Circuit gate -> [TestCase] -> IO Bool
ensureIO verbose eval c ts = and <$> mapM ensureIO' (zip [0::Int ..] ts)
  where
    ensureIO' (i, (inps, outs)) = do
        res <- eval c inps
        if res == outs then do
            let s = printf "test %d succeeded: input:%s expected:%s got:%s"
                            i (showInts inps) (showInts outs) (showInts res)
            when verbose (putStrLn s)
            return True
        else do
            let s = printf "test %d failed! input:%s expected:%s got:%s"
                            i (showInts inps) (showInts outs) (showInts res)
            putStrLn (red s)
            return False

foldCirc :: Gate gate => (gate -> [a] -> a) -> Circuit gate -> [a]
foldCirc f c = runIdentity (foldCircM f' c)
  where
    f' op _ xs = return (f op xs)

foldCircRef :: Gate gate => (gate -> Ref -> [a] -> a) -> Circuit gate -> [a]
foldCircRef f c = runIdentity (foldCircM f' c)
  where
    f' op ref xs = return (f op ref xs)

-- evaluate the circuit
foldCircM :: (Gate g, Monad m) => (g -> Ref -> [a] -> m a) -> Circuit g -> m [a]
foldCircM f c = foldCircRec f c (outputRefs c)

-- evaluate the circuit for a particular ref as output
foldCircRefM :: (Gate g, Monad m) => (g -> Ref -> [a] -> m a) -> Circuit g -> Ref -> m a
foldCircRefM !f !c !ref = head <$> foldCircRec f c [ref]

-- helper function for foldCircM and foldCircRefM
foldCircRec :: (Gate g, Monad m)
            => (g -> Ref -> [a] -> m a) -> Circuit g -> [Ref] -> m [a]
foldCircRec f c !outs = flip evalStateT IM.empty $ do
    forM_ (wires c) $ \(ref, gate) -> do
        argVals <- mapM look (gateArgs gate)
        val     <- lift (f gate ref argVals)
        modify' (IM.insert (getRef ref) val)
    mapM look outs
  where
    look r = gets (IM.lookup (getRef r)) >>= \case
        Nothing -> error "[foldCircRec] not topo sorted!"
        Just v  -> return v

dfs :: (Gate g, Monad m) => (g -> Ref -> [a] -> m a) -> Circuit g -> m [a]
dfs f c = evalStateT (mapM (dfsHelper f c) (outputRefs c)) IM.empty
  where
    dfsHelper :: (Gate g, Monad m)
                => (g -> Ref -> [a] -> m a) -> Circuit g -> Ref
                -> StateT (IM.IntMap a) m a
    dfsHelper f c !ref = do
        existingVal <- gets (IM.lookup (getRef ref))
        case existingVal of
            Just !val -> return val -- evaluated already
            Nothing   -> do
                let !g = c^.circ_refmap.at (getRef ref).non
                        (error $ printf "[foldCircMRec] no gate for ref %s!" (show ref))
                argVals <- mapM (dfsHelper f c) (gateArgs g)
                val     <- lift (f g ref argVals)
                modify' (IM.insert (getRef ref) val)
                return val

topoOrder :: Gate gate => Circuit gate -> [Ref]
topoOrder c = reverse $ execState (dfs eval c) []
  where
    eval :: gate -> Ref -> [a] -> State [Ref] ()
    eval _ ref _ = modify (ref:)

-- Ugly and fast!
topoLevels :: Gate gate => Circuit gate -> [[Ref]]
topoLevels c = nub $ map snd $ M.toAscList $ execState (dfs eval c) M.empty
  where
    eval :: gate -> Ref -> [Int] -> State (M.Map Int [Ref]) Int
    eval _ ref [] = modify (M.insertWith (++) 0 [ref]) >> return 0
    eval _ ref ds = do
        let d = 1 + maximum ds
        modify (M.insertWith (++) d [ref])
        return d

maxFanOut :: Gate gate => Circuit gate -> Int
maxFanOut = maximum . toListOf (circ_refcount . each)
