{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE Strict #-}
#endif

module Circuit where

import Circuit.Utils

import Control.Monad.Identity
import Control.Monad.Par (IVar, Par, runPar)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.ParallelIO
import Control.DeepSeq (NFData)
import Control.Monad.IfElse (whenM)
import Control.Monad.State.Strict
import Data.List (nub, sortBy)
import Lens.Micro.Platform
import Text.Printf
import qualified Control.Monad.Par as IVar
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import Debug.Trace

newtype Ref = Ref { getRef :: Int } deriving (Eq, Ord, NFData, Num)
newtype Id  = Id  { getId  :: Int } deriving (Eq, Ord, NFData, Num)

instance Show Ref where
    show ref = show (getRef ref)

instance Show Id where
    show id = show (getId id)

data Op = OpAdd !Ref !Ref
        | OpSub !Ref !Ref
        | OpMul !Ref !Ref
        | OpInput !Id
        | OpConst !Id
        deriving (Eq, Ord, Show)

data Circuit = Circuit
    { _circ_outputs     :: ![Ref]
    , _circ_inputs      :: ![Ref]
    , _circ_consts      :: !(M.Map Ref Id)
    , _circ_secret_refs :: !(IS.IntSet)
    , _circ_secret_ids  :: !(IS.IntSet)
    , _circ_refmap      :: !(IM.IntMap Op)
    , _circ_const_vals  :: !(M.Map Id Integer)
    , _circ_symlen      :: !Int
    , _circ_base        :: !Int -- the expected base of the inputs
    } deriving (Show)

makeLenses ''Circuit

type TestCase = ([Integer], [Integer])

--------------------------------------------------------------------------------
-- instances and such

emptyCirc :: Circuit
emptyCirc = Circuit [] [] M.empty IS.empty IS.empty IM.empty M.empty 1 2

getConst :: Circuit -> Id -> Integer
getConst c id = case c^.circ_const_vals.at id of
    Just x  -> x
    Nothing -> error ("[getConst] no const known for y" ++ show id)

secretConst :: Circuit -> Id -> Bool
secretConst c id = IS.member (getId id) (_circ_secret_ids c)

secretRefs :: Circuit -> [Ref]
secretRefs c = map Ref $ IS.toAscList (_circ_secret_refs c)

getGate :: Circuit -> Ref -> Op
getGate c ref = case c^.circ_refmap.at (getRef ref) of
    Nothing -> error (printf "[getGate] no ref %d!" (getRef ref))
    Just op -> op

randomizeSecrets :: Circuit -> IO Circuit
randomizeSecrets c = do
    key <- replicateM (nsecrets c) $ randIntegerModIO (fromIntegral (_circ_base c))
    let newSecrets = M.fromList $ zip (map Id $ IS.toAscList (c^.circ_secret_ids)) key
    return $ c & circ_const_vals %~ M.union newSecrets

genTest :: Circuit -> IO TestCase
genTest c
    | c^.circ_symlen == 1 = do
        let q = (fromIntegral (_circ_base c) :: Integer) ^ (fromIntegral (ninputs c) :: Integer)
        inp <- num2Base (_circ_base c) (ninputs c) <$> randIO (randIntegerMod q)
        return (inp, plainEval c inp)
    | otherwise = do
        when ((ninputs c `mod` _circ_symlen c) /= 0) $
            error "[genTest] inputs not evenly dividable"
        let nsyms = ninputs c `div` _circ_symlen c
        inp <- fmap concat $ replicateM nsyms $ do
            x <- fromIntegral <$> randIO (randInteger (numBits (c^.circ_symlen)))
            return [ if i == x then 1 else 0 | i <- [0..c^.circ_symlen-1] ]
        return (inp, plainEval c inp)


printCircInfo :: Circuit -> IO ()
printCircInfo c = do
    -- let ds = degs c
    let n = ninputs c
    printf "circuit info\n"
    printf "============\n"
    printf "ninputs=%d noutputs=%d nconsts=%d nsecrets=%d\n" n (noutputs c) (nconsts c) (nsecrets c)
    printf "symlen=%d base=%d\n" (symlen c) (c^.circ_base)
    printf "ngates=%d depth=%d\n" (ngates c) (depth c)
    printf "degree=%d\n" (circDegree c)

printTruthTable :: Circuit -> IO ()
printTruthTable c = forM_ inputs $ \inp -> do
    let out = plainEval c inp
    printf "%s -> %s\n" (showInts inp) (showInts out)
  where
    n = ninputs c `div` symlen c
    sym x = [ if i == x then (1 :: Integer) else 0 | i <- [ 0 .. symlen c - 1 ] ]
    inputs = case symlen c of
        1 -> sequence (replicate (ninputs c) [(0::Integer)..fromIntegral (c^.circ_base - 1)])
        _ -> map concat $ sequence (replicate n (map sym [0..symlen c - 1]))

circEq :: Circuit -> Circuit -> IO Bool
circEq c0 c1
  | ninputs  c0 /= ninputs  c1 = return False
  | noutputs c0 /= noutputs c1 = return False
  | otherwise = do
    let n = 10
    t0 <- replicateM n (genTest c0)
    t1 <- replicateM n (genTest c1)
    x  <- ensure False c1 t0
    y  <- ensure False c0 t1
    return (x && y)

opArgs :: Op -> [Ref]
opArgs (OpAdd x y)  = [x,y]
opArgs (OpSub x y)  = [x,y]
opArgs (OpMul x y)  = [x,y]
opArgs (OpInput _) = []
opArgs (OpConst _) = []

ngates :: Circuit -> Int
ngates = IM.size . _circ_refmap

ninputs :: Circuit -> Int
ninputs = length . _circ_inputs

nconsts :: Circuit -> Int
nconsts = length . _circ_consts

nsecrets :: Circuit -> Int
nsecrets = IS.size . _circ_secret_refs

noutputs :: Circuit -> Int
noutputs = length . _circ_outputs

symlen :: Circuit -> Int
symlen = _circ_symlen

ydeg :: Circuit -> Integer
ydeg c = head (degs c)

xdeg :: Circuit -> Int -> Integer
xdeg c i = degs c !! (i+1)

degs :: Circuit -> [Integer]
degs c = map (varDegree c) ids
  where
    ids = OpConst (Id (-1)) : map (OpInput . Id) [0 .. ninputs c-1]

depth :: Circuit -> Integer
depth c = maximum $ foldCirc f c
  where
    f (OpInput _) [] = 0
    f (OpConst _) [] = 0
    f _           xs = maximum xs + 1

varDegree :: Circuit -> Op -> Integer
varDegree c z = maximum (varDegree' c z)

varDegree' :: Circuit -> Op -> [Integer]
varDegree' c z = foldCirc f c
  where
    f (OpAdd _ _) [x,y] = max x y
    f (OpSub _ _) [x,y] = max x y
    f (OpMul _ _) [x,y] = x + y

    f x _ = if eq x z then 1 else 0

    eq (OpInput x) (OpInput y) = x == y
    eq (OpConst _) (OpConst _) = True
    eq _ _ = False

circDegree :: Circuit -> Integer
circDegree c = maximum $ foldCirc f c
  where
    f (OpAdd _ _) [x,y] = max x y
    f (OpSub _ _) [x,y] = max x y
    f (OpMul _ _) [x,y] = x + y
    f (OpInput _) _ = 1
    f (OpConst _) _ = 1

-- evaluate the circuit using an arbitrary integral type as input
evalMod :: (Show a, Integral a) => Circuit -> [a] -> a -> [a]
evalMod c inps q = foldCirc eval c
  where
    eval (OpAdd _ _) [x,y] = x + y % q
    eval (OpSub _ _) [x,y] = x - y % q
    eval (OpMul _ _) [x,y] = x * y % q
    eval (OpInput i) [] = inps !! getId i
    eval (OpConst i) [] = fromIntegral (getConst c i)
    eval op args  = error ("[evalMod] weird input: " ++ show op ++ " " ++ show args)

zeroTest :: Integer -> Integer
zeroTest 0 = 0
zeroTest _ = 1

plainEval :: Circuit -> [Integer] -> [Integer]
plainEval c inps
    | ninputs c /= length inps =
        error (printf "[plainEval] incorrect number of inputs: expected %d, got %s" (ninputs c) (show inps))
    | otherwise = map zeroTest $ foldCirc eval c
  where
    eval :: Op -> [Integer] -> Integer
    eval (OpAdd _ _) [x,y] = x + y
    eval (OpSub _ _) [x,y] = x - y
    eval (OpMul _ _) [x,y] = x * y
    eval (OpInput i) [] = inps !! getId i
    eval (OpConst i) [] = getConst c i
    eval op args = error ("[plainEval] weird input: " ++ show op ++ " " ++ show args)

parEval :: Circuit -> [Integer] -> [Integer]
parEval c inps = map zeroTest $ runPar $ mapM IVar.get =<< foldCircM eval c
  where
    eval :: Op -> Ref -> [IVar Integer] -> Par (IVar Integer)
    eval (OpAdd _ _) _ [x,y] = liftBin (+) x y
    eval (OpSub _ _) _ [x,y] = liftBin (-) x y
    eval (OpMul _ _) _ [x,y] = liftBin (*) x y
    eval (OpInput i) _ [] = IVar.newFull (inps !! getId i)
    eval (OpConst i) _ [] = IVar.newFull (getConst c i)
    eval op _ args = error ("[parEval] weird input: " ++ show op ++ " with " ++ show (length args) ++ " arguments")

    liftBin f x y = IVar.spawn (liftM2 f (IVar.get x) (IVar.get y))

plainEvalIO :: Circuit -> [Integer] -> IO [Integer]
plainEvalIO c xs = map zeroTest <$> foldCircIO eval c
  where
    eval :: Op -> [Integer] -> Integer
    eval (OpAdd _ _) [x,y] = x + y
    eval (OpSub _ _) [x,y] = x - y
    eval (OpMul _ _) [x,y] = x * y
    eval (OpInput i) [] = xs !! getId i
    eval (OpConst i) [] = getConst c i
    eval op args = error ("[plainEval] weird input: " ++ show op ++ " " ++ show args)

ensure :: Bool -> Circuit -> [TestCase] -> IO Bool
ensure verbose c ts = and <$> mapM ensure' (zip [(0::Integer)..] ts)
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


ensureIO :: Bool -> (Circuit -> [Integer] -> IO [Integer]) -> Circuit -> [TestCase] -> IO Bool
ensureIO verbose eval c ts = and <$> mapM ensureIO' (zip [(0::Integer)..] ts)
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

foldCirc :: (Op -> [a] -> a) -> Circuit -> [a]
foldCirc f c = runIdentity (foldCircM f' c)
  where
    f' op _ xs = return (f op xs)

foldCircRef :: (Op -> Ref -> [a] -> a) -> Circuit -> [a]
foldCircRef f c = runIdentity (foldCircM f' c)
  where
    f' op ref xs = return (f op ref xs)

foldCircM :: Monad m => (Op -> Ref -> [a] -> m a) -> Circuit -> m [a]
foldCircM f c = evalStateT (mapM eval (c^.circ_outputs)) M.empty
  where
    eval ref = gets (M.lookup ref) >>= \case
        Just val -> return val
        Nothing  -> do
            when (IM.notMember (getRef ref) (c^.circ_refmap))
                (traceM (printf "unknown ref \"%s\"" (show ref)))
            let op = c^.circ_refmap.at (getRef ref).non (error "no ref")
            argVals <- mapM eval (opArgs op)
            val     <- lift (f op ref argVals)
            modify (M.insert ref val)
            return val


foldCircRefM :: Monad m => (Op -> Ref -> [a] -> m a) -> Circuit -> Ref -> m a
foldCircRefM f c ref = evalStateT (eval ref) M.empty
  where
    eval ref = gets (M.lookup ref) >>= \case
        Just val -> return val
        Nothing  -> do
            when (IM.notMember (getRef ref) (c^.circ_refmap))
                (traceM (printf "unknown ref \"%s\"" (show ref)))
            let op = c^.circ_refmap.at (getRef ref).non (error "no ref")
            argVals <- mapM eval (opArgs op)
            val     <- lift (f op ref argVals)
            modify (M.insert ref val)
            return val

-- evaluate the circuit in parallel
foldCircIO :: NFData a => (Op -> [a] -> a) -> Circuit -> IO [a]
foldCircIO f c = do
    let refs = map Ref $ IM.keys (_circ_refmap c)
    mem <- (M.fromList . zip refs) <$> replicateM (length refs) newEmptyTMVarIO
    let eval :: Ref -> IO ()
        eval ref = do
            let op      = c^.circ_refmap.at (getRef ref).non (error "no ref")
                argRefs = map (mem M.!) (opArgs op)
            -- this condition should never be hit since we parallelize over the topological levels
            whenM (or <$> mapM (atomically . isEmptyTMVar) argRefs) $ do
                putStrLn "blocking!"
                yield
                eval ref
            argVals <- mapM (atomically . readTMVar) argRefs
            let val = f op argVals
            forceM val
            atomically $ putTMVar (mem M.! ref) val
    let lvls = topoLevels c
    forceM lvls
    forM_ (zip [(0 :: Int)..] lvls) $ \(_, lvl) -> do
        {-printf "evaluating level %d size=%d\n" i (length lvl)-}
        parallelInterleaved (map eval lvl)
    mapM (atomically . readTMVar . (mem M.!)) (_circ_outputs c)

topologicalOrder :: Circuit -> [Ref]
topologicalOrder c = reverse $ execState (foldCircM eval c) []
  where
    eval :: Op -> Ref -> [a] -> State [Ref] ()
    eval _ ref _ = modify (ref:)

-- Ugly and fast!
topoLevels :: Circuit -> [[Ref]]
topoLevels c = nub $ map snd $ M.toAscList $ execState (foldCircM eval c) M.empty
  where
    eval :: Op -> Ref -> [Int] -> State (M.Map Int [Ref]) Int
    eval _ ref [] = modify (M.insertWith (++) 0 [ref]) >> return 0
    eval _ ref ds = do
        let d = 1 + maximum ds
        modify (M.insertWith (++) d [ref])
        return d

sortGates :: Circuit -> [Ref]
sortGates c = concatMap (sortBy refDist) (topoLevels c)
  where
    refDist xref yref = let xargs = opArgs (getGate c xref)
                            yargs = opArgs (getGate c yref)
                        in case (xargs, yargs) of
                            ([], []) -> EQ
                            (_,  []) -> GT
                            ([], _ ) -> LT
                            (xs, ys) -> let cs = zipWith compare xs ys
                                        in if any (== EQ) cs then EQ else maximum cs

gates :: Circuit -> [(Ref, Op)]
gates c = filter (gate.snd) $ map (\ref -> (ref, getGate c ref)) (topologicalOrder c)
  where
    gate (OpInput _) = False
    gate (OpConst _) = False
    gate _ = True

gateRefs :: Circuit -> [Ref]
gateRefs = map fst . gates

sortedNonInputGates :: Circuit -> [Ref]
sortedNonInputGates c = filter notInput (sortGates c)
  where
    notInput ref = notElem ref (_circ_inputs c) &&
                   IS.notMember (getRef ref) (_circ_secret_refs c)

intermediateGates :: Circuit -> [Ref]
intermediateGates c = filter intermediate (topologicalOrder c)
  where
    intermediate ref = notElem ref (_circ_inputs c) &&
                       notElem ref (_circ_outputs c) &&
                       IS.notMember (getRef ref) (_circ_secret_refs c)

numDisjointAdditions :: Circuit -> Int
numDisjointAdditions c = execState (foldCircM eval c) 0
  where
    eval :: Op -> Ref -> [M.Map Op Int] -> State Int (M.Map Op Int)
    eval (OpAdd _ _) _ [xdegs, ydegs] = do
        let degs = M.unionWith max xdegs ydegs
        when (M.null (M.intersection xdegs ydegs)) (modify succ)
        return degs
    eval (OpSub _ _) _ [xdegs, ydegs] = do
        let degs  = M.unionWith max xdegs ydegs
        return degs
    eval (OpMul _ _) _ [xdegs, ydegs] = do
        let degs  = M.unionWith (+) xdegs ydegs
        return degs
    eval op@(OpInput _) _ _ = return (M.singleton op 1)
    eval op@(OpConst id) _ _ = if secretConst c id
                                   then return (M.singleton op 1)
                                   else return M.empty
    eval _ _ _ = undefined
