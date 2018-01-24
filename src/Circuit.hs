{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}

module Circuit
    ( module Circuit.Types
    , module Circuit
    ) where

import Circuit.Types
import Circuit.Utils

import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.List (nub)
import Lens.Micro.Platform
import Text.Printf
import qualified Data.Array as A
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V

--------------------------------------------------------------------------------
-- Generic circuit functions

emptyCirc :: Circuit a
emptyCirc = Circuit
    { _circ_outputs        = []
    , _circ_inputs         = IM.empty
    , _circ_consts         = IM.empty
    , _circ_secrets        = IM.empty
    , _circ_refmap         = IM.empty
    , _circ_const_vals     = IM.empty
    , _circ_secret_vals    = IM.empty
    , _circ_symlen         = IM.empty
    , _circ_refcount       = IM.empty
    , _circ_maxref         = 0
    , _circ_sigma_vecs     = IS.empty
    }

nwires :: Circuit gate -> Int
nwires = IM.size . view circ_refmap

ngates :: Circuit gate -> Int
ngates c = nwires c - ninputs c - nconsts c

ninputs :: Circuit gate -> Int
ninputs = IM.size . view circ_inputs

nconsts :: Circuit gate -> Int
nconsts = length . view circ_consts

nsecrets :: Circuit gate -> Int
nsecrets = IM.size . view circ_secrets

nsymbols :: Circuit gate -> Int
nsymbols = IM.size . view circ_symlen

noutputs :: Circuit gate -> Int
noutputs = length . view circ_outputs

symlen :: Circuit gate -> Int -> Int
symlen c i = c ^. circ_symlen . at i . non (error $ "[symlen] no symbol " ++ show i)

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
inputRefs c = IM.elems (c^.circ_inputs)

constRefs :: Gate gate => Circuit gate -> [Ref]
constRefs c = IM.elems (c^.circ_consts)

secretRefs :: Gate gate => Circuit gate -> [Ref]
secretRefs c = IM.elems (c^.circ_secrets)

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

getConst :: Circuit gate -> ConstId -> Int
getConst c id = case c^.circ_const_vals.at (getConstId id) of
    Just x  -> x
    Nothing -> error ("[getConst] no const known for const " ++ show id)

getSecret :: Circuit gate -> SecretId -> Int
getSecret c id = case c^.circ_secret_vals.at (getSecretId id) of
    Just x  -> x
    Nothing -> error ("[getSecret] no secret known for secret " ++ show id)

secretIds :: Circuit gate -> [SecretId]
secretIds = map SecretId . IM.keys . view circ_secrets

getGate :: Circuit gate -> Ref -> gate
getGate c ref = case c^.circ_refmap.at (getRef ref) of
    Nothing   -> error (printf "[getGate] no ref %d!" (getRef ref))
    Just gate -> gate

randomizeSecrets :: Circuit gate -> IO (Circuit gate)
randomizeSecrets c = do
    key <- randIO $ randIntsMod (nsecrets c) 2
    let newSecrets = IM.fromList $ zip (map getSecretId (secretIds c)) key
    return $ c & circ_const_vals %~ IM.union newSecrets

genTest :: Gate gate => Circuit gate -> IO TestCase
genTest c
    | all (==1) (c^.circ_symlen) = do
        inp <- randIO $ randIntsMod (ninputs c) 2
        return (inp, plainEval c inp)
    | otherwise = do
        inp <- fmap concat $ randIO $ forM (IM.toList (c^.circ_symlen)) $ \(i,len) -> do
            if IS.member i (c^.circ_sigma_vecs) then do
                x <- randIntMod len
                return [ if j == x then 1 else 0 | j <- [0..len-1] ]
            else do
                randIntsMod len 2
        return (inp, plainEval c inp)


printCircInfo :: Gate g => Circuit g -> IO ()
printCircInfo c = do
    let n = ninputs c
    printf "\tcircuit info\n"
    printf "\t============\n"
    printf "\tninputs=%d noutputs=%d nconsts=%d nsecrets=%d nwires=%d\n"
           n (noutputs c) (nconsts c) (nsecrets c) (nwires c)
    printf "\tdepth=%d degree=%d\n" (depth c) (circDegree c)
    printf "\tsymlens = %s\n" (unwords (map show (IM.elems (c^.circ_symlen))))
    printf "\tsigmas  = %s\n" (unwords (map (show . (b2i :: Bool -> Int)
                                    . flip IS.member (c^.circ_sigma_vecs)) [0..nsymbols c-1]))
    printf "\tconsts  = %s\n" (unwords (map show (IM.elems (c^.circ_const_vals))))
    printf "\tsecrets = %s\n" (unwords (map show (IM.elems (c^.circ_secret_vals))))

printTruthTable :: Gate gate => Circuit gate -> IO ()
printTruthTable c = do
    let allCombos = map concat $ sequence $ map (possibleInputs c) ([0..nsymbols c-1] :: [Int])
    forM_ allCombos $ \inp -> do
        let out = plainEval c inp
        printf "%s -> %s\n" (showInts inp) (showInts out)
  where
    possibleInputs c i = case symlen c i of
        1   -> [[0],[1]]
        len -> if IS.member i (c^.circ_sigma_vecs)
                    then map (sigma len) [0..len-1]
                    else permutations len [0,1]
    sigma len x = [ if i == x then 1 else 0 | i <- [ 0 .. len - 1 ] ]

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

-- TODO: make these degree functions understand symbols
degs :: Gate g => Circuit g -> [Integer]
degs c = map (maxVarDegree c) vars
  where
    vars = Const (ConstId (-1)) : map (Input . InputId) [0 .. ninputs c-1]

depth :: Gate gate => Circuit gate -> Integer
depth c = maximum $ foldCirc f c
  where
    f _ args = if null args then 0 else maximum args + 1

maxVarDegree :: Gate g => Circuit g -> BaseGate -> Integer
maxVarDegree c z = maximum (varDegree c z)

varDegree :: (Gate g) => Circuit g -> BaseGate -> [Integer]
varDegree c z = foldCirc f c
  where
    f g args = case gateGetBase g of
        Just b  -> if eq b z then 1 else 0
        Nothing -> if gateIsMul g then sum args else maximum args

    eq (Input x) (Input y) = x == y
    eq (Const _)  (Const _)  = True
    eq (Const _)  (Secret _) = True
    eq (Secret _) (Const _)  = True
    eq (Secret _) (Secret _) = True
    eq _ _ = False

circDegree :: Gate g => Circuit g -> Integer
circDegree c = maximum $ foldCirc f c
  where
    f g [x] = x
    f _ []  = 1 -- input or const
    f g args = if gateIsMul g
                  then sum args
                  else maximum args

zeroTest :: Int -> Int
zeroTest 0 = 0
zeroTest _ = 1

plainEval :: Gate gate => Circuit gate -> [Int] -> [Int]
plainEval c inps
    | ninputs c /= length inps =
        error (printf "[plainEval] incorrect number of inputs: expected %d, got %s"
                      (ninputs c) (show inps))
    | otherwise = map zeroTest $ foldCirc (gateEval getBase) c
  where
    inps' = A.listArray (0 :: InputId, InputId (length inps-1)) inps
    getBase (Input id)  = inps' A.! id
    getBase (Const id)  = getConst c id
    getBase (Secret id) = getSecret c id

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
