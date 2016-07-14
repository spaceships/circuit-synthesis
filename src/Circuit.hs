{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Circuit where

import Util
import Rand

import Control.Monad.Identity
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.ParallelIO
import Control.DeepSeq (NFData)
import Control.Monad.IfElse (whenM)
import Control.Monad.State.Strict
import Data.Map.Strict ((!))
import Text.Printf
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Bimap as B

import Debug.Trace

newtype Ref = Ref { getRef :: Int } deriving (Eq, Ord, NFData, Num)
newtype Id  = Id  { getId  :: Int } deriving (Eq, Ord, NFData, Num)

data Op = OpAdd Ref Ref
        | OpSub Ref Ref
        | OpMul Ref Ref
        | OpInput Id
        | OpSecret Id
        deriving (Eq, Ord, Show)

data Circuit = Circuit {
      circ_outputs     :: [Ref]
    , circ_inputs      :: [Ref]
    , circ_secrets     :: M.Map Id Integer
    , circ_secret_refs :: M.Map Ref Id
    , circ_refmap      :: M.Map Ref Op
    , circ_consts      :: B.Bimap Integer Ref
    } deriving (Show)

type TestCase = ([Bool], [Bool])

--------------------------------------------------------------------------------
-- instances and such

emptyCirc = Circuit [] [] M.empty M.empty M.empty B.empty

instance Show Ref where
    show ref = "r" ++ show (getRef ref)

instance Show Id where
    show id = "i" ++ show (getId id)

getSecret :: Circuit -> Id -> Integer
getSecret c id = case M.lookup id (circ_secrets c) of
    Just x  -> x
    Nothing -> error ("[getSecret] no secret known for y" ++ show id)

-- TODO: generate random keys somehow too
genTest :: Circuit -> IO TestCase
genTest c = do
    inp <- num2Bits (ninputs c) <$> randIO (randInteger (ninputs c))
    return (inp, plainEval c inp)

printCircuitInfo :: Circuit -> IO ()
printCircuitInfo c = do
    let ds = degs c
    printf "circuit info: depth=%d ninputs=%d noutputs=%d nconsts=%d%s ngates=%d\n"
            (depth c) (ninputs c) (noutputs c) (nconsts c)
            (show (M.elems (circ_secrets c)))
            (ngates c)
    printf "degs=%s var-degree=%d circ-degree=%d\n" (show ds) (sum ds) (circDegree c)

printCircuitInfoFreeNot :: Circuit -> IO ()
printCircuitInfoFreeNot c = do
    let ds = degs' True c
    printf "circuit info: depth=%d ninputs=%d noutputs=%d nconsts=%d[%d] ngates=%d\n"
            (depth c) (ninputs c) (noutputs c) (nconsts c) (nsecrets c) (ngates c)
    printf "degs=%s total degree=%d\n" (show ds) (sum ds)

printTruthTable :: Circuit -> IO ()
printTruthTable c = forM_ inputs $ \inp -> do
    let outp = plainEval c inp
    printf "%s -> %s\n" (showBits' inp) (showBits' outp)
  where
    inputs = sequence (replicate (ninputs c) [False, True])

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
opArgs (OpSecret _) = []
opArgs (OpInput  _) = []

ngates :: Circuit -> Int
ngates = M.size . circ_refmap

ninputs :: Circuit -> Int
ninputs = length . circ_inputs

nconsts :: Circuit -> Int
nconsts = length . circ_secret_refs

nsecrets :: Circuit -> Int
nsecrets = M.size . circ_secrets

noutputs :: Circuit -> Int
noutputs = length . circ_outputs

ydeg :: Circuit -> Int
ydeg c = head (degs c)

xdeg :: Circuit -> Int -> Int
xdeg c i = degs c !! (i+1)

degs :: Circuit -> [Int]
degs = degs' False

-- with optional Free Not optimization
degs' :: Bool -> Circuit -> [Int]
degs' freeNot c = map (varDegree' freeNot c) ids
  where
    ids = OpSecret (Id (-1)) : map (OpInput . Id) [0 .. ninputs c-1]

depth :: Circuit -> Int
depth c = maximum $ foldCirc f c
  where
    f (OpInput  _) [] = 0
    f (OpSecret _) [] = 0
    f _            xs = maximum xs + 1

varDegree :: Circuit -> Op -> Int
varDegree = varDegree' False

-- with optional FreeNot optimization
varDegree' :: Bool -> Circuit -> Op -> Int
varDegree' freeNot c z = maximum $ foldCirc f c
  where
    f (OpAdd _ _) [x,y] = max x y
    f (OpSub z _) [x,y] = if freeNot && (circ_refmap c M.! z == OpSecret 0)
                             then y
                             else max x y
    f (OpMul _ _) [x,y] = x + y

    f x _ = if eq x z then 1 else 0

    eq (OpInput  x) (OpInput  y) = x == y
    eq (OpSecret _) (OpSecret _) = True
    eq _ _ = False

-- TODO make me better! does this really reflect the degree of the multivariate
-- polynomial corresponding to C?
circDegree :: Circuit -> Int
circDegree c = maximum $ foldCirc f c
  where
    f (OpAdd _ _) [x,y] = max x y
    f (OpSub _ _) [x,y] = max x y
    f (OpMul _ _) [x,y] = x + y
    f (OpInput  id) _ = 1
    f (OpSecret id) _ = 1
    f _ _ = error "[circDegree] unknown input"

-- evaluate the circuit using an arbitrary integral type as input
evalMod :: (Show a, Integral a) => Circuit -> [a] -> a -> [a]
evalMod c inps q = foldCirc eval c
  where
    eval (OpAdd _ _) [x,y] = x + y % q
    eval (OpSub _ _) [x,y] = x - y % q
    eval (OpMul _ _) [x,y] = x * y % q
    eval (OpInput  i) [] = inps !! getId i
    eval (OpSecret i) [] = fromIntegral (getSecret c i)
    eval op args  = error ("[evalMod] weird input: " ++ show op ++ " " ++ show args)

plainEval :: Circuit -> [Bool] -> [Bool]
plainEval c inps = map (/= 0) (foldCirc eval c)
  where
    eval :: Op -> [Integer] -> Integer
    eval (OpAdd _ _) [x,y] = x + y
    eval (OpSub _ _) [x,y] = x - y
    eval (OpMul _ _) [x,y] = x * y
    eval (OpInput  i) [] = b2i (inps !! getId i)
    eval (OpSecret i) [] = getSecret c i
    eval op args = error ("[plainEval] weird input: " ++ show op ++ " " ++ show args)

plainEvalIO :: Circuit -> [Bool] -> IO [Bool]
plainEvalIO c xs = do
    zs <- foldCircIO eval c
    return $ map (/= 0) zs
  where
    eval :: Op -> [Integer] -> Integer
    eval (OpAdd _ _) [x,y] = x + y
    eval (OpSub _ _) [x,y] = x - y
    eval (OpMul _ _) [x,y] = x * y
    eval (OpInput  i) [] = b2i (xs !! getId i)
    eval (OpSecret i) [] = getSecret c i
    eval op args = error ("[plainEval] weird input: " ++ show op ++ " " ++ show args)

ensure :: Bool -> Circuit -> [TestCase] -> IO Bool
ensure verbose c ts = and <$> mapM ensure' (zip [(0::Int)..] ts)
  where
    ensure' (i, (inps, outs)) = do
        let res = plainEval c inps
        if res == outs then do
            let s = printf "test %d succeeded: input:%s expected:%s got:%s"
                            i (showBits inps) (showBits outs) (showBits res)
            when verbose (putStrLn s)
            return True
        else do
            let s = printf "test %d failed! input:%s expected:%s got:%s"
                            i (showBits inps) (showBits outs) (showBits res)
            putStrLn (red s)
            return False


ensureIO :: Bool -> (Circuit -> [Bool] -> IO [Bool]) -> Circuit -> [TestCase] -> IO Bool
ensureIO verbose eval c ts = and <$> mapM ensureIO' (zip [(0::Int)..] ts)
  where
    ensureIO' (i, (inps, outs)) = do
        res <- eval c inps
        if res == outs then do
            let s = printf "test %d succeeded: input:%s expected:%s got:%s"
                            i (showBits inps) (showBits outs) (showBits res)
            when verbose (putStrLn s)
            return True
        else do
            let s = printf "test %d failed! input:%s expected:%s got:%s"
                            i (showBits inps) (showBits outs) (showBits res)
            putStrLn (red s)
            return False

foldCirc :: (Op -> [a] -> a) -> Circuit -> [a]
foldCirc f c = runIdentity (foldCircM f' c)
  where
    f' op _ xs = return (f op xs)

foldCircM :: Monad m => (Op -> Ref -> [a] -> m a) -> Circuit -> m [a]
foldCircM f c = evalStateT (mapM eval (circ_outputs c)) M.empty
  where
    eval ref = gets (M.lookup ref) >>= \case
        Just val -> return val
        Nothing  -> do
            let op = circ_refmap c ! ref
            argVals <- mapM eval (opArgs op)
            val     <- lift (f op ref argVals)
            modify (M.insert ref val)
            return val

-- evaluate the circuit in parallel
foldCircIO :: NFData a => (Op -> [a] -> a) -> Circuit -> IO [a]
foldCircIO f c = do
    let refs = M.keys (circ_refmap c)
    mem <- (M.fromList . zip refs) <$> replicateM (length refs) newEmptyTMVarIO
    let eval :: Ref -> IO ()
        eval ref = do
            let op      = circ_refmap c ! ref
                argRefs = map (mem!) (opArgs op)
            -- this condition should never be hit since we parallelize over the topological levels
            whenM (or <$> mapM (atomically . isEmptyTMVar) argRefs) $ do
                putStrLn "blocking!"
                yield
                eval ref
            argVals <- mapM (atomically . readTMVar) argRefs
            let val = f op argVals
            forceM val
            atomically $ putTMVar (mem ! ref) val
    let lvls = topoLevels c
    forceM lvls
    forM_ (zip [(0 :: Int)..] lvls) $ \(_, lvl) -> do
        {-printf "evaluating level %d size=%d\n" i (length lvl)-}
        parallelInterleaved (map eval lvl)
    mapM (atomically . readTMVar . (mem !)) (circ_outputs c)

topologicalOrder :: Circuit -> [Ref]
topologicalOrder c = reverse $ execState (foldCircM eval c) []
  where
    eval :: Op -> Ref -> [a] -> State [Ref] ()
    eval _ ref _ = modify (ref:)

topoLevels :: Circuit -> [[Ref]]
topoLevels c = map S.toAscList lvls
  where
    topo = topologicalOrder c
    lvls = execState (mapM_ eval topo) []

    eval :: Ref -> State [S.Set Ref] ()
    eval ref = modify (putRef ref)

    putRef :: Ref -> [S.Set Ref] -> [S.Set Ref]
    putRef ref []     = [S.singleton ref]
    putRef ref (x:xs) = if not (any (`S.member` x) (dependencies ref))
                            then S.insert ref x : xs
                            else x : putRef ref xs

    dependencies :: Ref -> [Ref]
    dependencies ref = case circ_refmap c ! ref of
        OpInput  _ -> []
        OpSecret _ -> []
        op -> opArgs op ++ concatMap dependencies (opArgs op)

intermediateGates :: Circuit -> [Ref]
intermediateGates c = filter intermediate (topologicalOrder c)
  where
    intermediate ref = notElem ref (circ_inputs c) &&
                       notElem ref (circ_outputs c) &&
                       M.notMember ref (circ_secret_refs c)
