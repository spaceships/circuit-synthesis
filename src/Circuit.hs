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

import Debug.Trace

newtype Ref = Ref { getRef :: Int } deriving (Eq, Ord, NFData, Num)
newtype Id  = Id  { getId  :: Int } deriving (Eq, Ord, NFData, Num)

data Op = OpAdd Ref Ref
        | OpSub Ref Ref
        | OpMul Ref Ref
        | OpConst Id
        | OpInput Id
        deriving (Eq, Ord, Show)

data Circuit = Circuit {
      circ_outputs :: [Ref]
    , circ_inputs  :: [Ref]
    , circ_consts  :: [Ref]
    , circ_secrets :: M.Map Id Integer
    , circ_refmap  :: M.Map Ref Op
    } deriving (Show)

type TestCase = ([Bool], [Bool])

--------------------------------------------------------------------------------
-- instances and such

emptyCirc = Circuit [] [] [] M.empty M.empty

instance Show Ref where
    show ref = "r" ++ show (getRef ref)

instance Show Id where
    show id = "i" ++ show (getId id)

getSecret :: Circuit -> Id -> Integer
getSecret c id = case M.lookup id (circ_secrets c) of
    Just x  -> x
    Nothing -> error ("[getSecret] no secret known for y" ++ show id)

genTest :: Circuit -> IO TestCase
genTest c = do
    inp <- num2Bits (ninputs c) <$> randIO (randInteger (ninputs c))
    return (inp, plainEval c inp)

genTestStr :: Circuit -> IO (String, String)
genTestStr c = do
    (inp, out) <- genTest c
    return (showBits inp, showBits out)

printCircuitInfo :: Circuit -> IO ()
printCircuitInfo c = do
    let ds = degs c
    printf "circuit info: depth=%d ninputs=%d noutputs=%d nconsts=%d[%d] ngates=%d\n"
            (depth c) (ninputs c) (noutputs c) (nconsts c) (nsecrets c) (ngates c)
    printf "degs=%s total degree=%d\n" (show ds) (sum ds)

opArgs :: Op -> [Ref]
opArgs (OpAdd x y) = [x,y]
opArgs (OpSub x y) = [x,y]
opArgs (OpMul x y) = [x,y]
opArgs (OpConst _) = []
opArgs (OpInput _) = []

ngates :: Circuit -> Int
ngates = M.size . circ_refmap

ninputs :: Circuit -> Int
ninputs = length . circ_inputs

nconsts :: Circuit -> Int
nconsts = length . circ_consts

nsecrets :: Circuit -> Int
nsecrets = M.size . circ_secrets

noutputs :: Circuit -> Int
noutputs = length . circ_outputs

ydeg :: Circuit -> Int
ydeg c = head (degs c)

xdeg :: Circuit -> Int -> Int
xdeg c i = degs c !! (i+1)

degs :: Circuit -> [Int]
degs c = map (varDegree c) ids
  where
    ids = OpConst (Id (-1)) : map (OpInput . Id) [0 .. ninputs c - 1]

depth :: Circuit -> Int
depth c = maximum $ foldCirc f c
  where
    f (OpInput _) []  = 0
    f (OpConst _) []  = 0
    f _           xs  = maximum xs + 1

varDegree :: Circuit -> Op -> Int
varDegree c z = maximum $ foldCirc f c
  where
    f (OpAdd _ _) [x,y] = max x y
    f (OpSub _ _) [x,y] = max x y
    f (OpMul _ _) [x,y] = x + y

    f x _ = if eq x z then 1 else 0

    eq (OpInput x) (OpInput y) = x == y
    eq (OpConst _) (OpConst _) = True
    eq _         _             = False

-- TODO make me better! does this really reflect the degree of the multivariate
-- polynomial corresponding to C?
circDegree :: Circuit -> Int
circDegree c = maximum $ foldCirc f c
  where
    f (OpAdd _ _) [x,y] = max x y
    f (OpSub _ _) [x,y] = max x y
    f (OpMul _ _) [x,y] = x + y
    f (OpInput id) _ = 1
    f (OpConst id) _ = 1

evalMod :: (Show a, Integral a) => Circuit -> [a] -> a -> [a]
evalMod c inps q = foldCirc eval c
  where
    eval (OpAdd _ _) [x,y] = x + y % q
    eval (OpSub _ _) [x,y] = x - y % q
    eval (OpMul _ _) [x,y] = x * y % q
    eval (OpInput i) []    = inps !! getId i
    eval (OpConst i) []    = fromIntegral (getSecret c i)
    eval op        args  = error ("[evalMod] weird input: " ++ show op ++ " " ++ show args)

plainEval :: Circuit -> [Bool] -> [Bool]
plainEval c inps = map (/= 0) (foldCirc eval c)
  where
    eval :: Op -> [Integer] -> Integer
    eval (OpAdd _ _) [x,y] = x + y
    eval (OpSub _ _) [x,y] = x - y
    eval (OpMul _ _) [x,y] = x * y
    eval (OpInput i) []    = b2i (inps !! getId i)
    eval (OpConst i) []    = getSecret c i
    eval op        args  = error ("[plainEval] weird input: " ++ show op ++ " " ++ show args)

plainEvalIO :: Circuit -> [Bool] -> IO [Bool]
plainEvalIO c xs = do
    zs <- foldCircIO eval c
    return $ map (/= 0) zs
  where
    eval :: Op -> [Integer] -> Integer
    eval (OpAdd _ _) [x,y] = x + y
    eval (OpSub _ _) [x,y] = x - y
    eval (OpMul _ _) [x,y] = x * y
    eval (OpInput i) []    = b2i (xs !! getId i)
    eval (OpConst i) []    = getSecret c i
    eval op        args  = error ("[plainEval] weird input: " ++ show op ++ " " ++ show args)

ensure :: Bool -> (Circuit -> [Bool] -> IO [Bool]) -> Circuit -> [TestCase] -> IO Bool
ensure verbose eval c ts = and <$> mapM ensure' (zip [(0::Int)..] ts)
  where
    toBit :: Bool -> Char
    toBit b = if b then '1' else '0'

    ensure' (i, (inps, outs)) = do
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
        OpInput _ -> []
        OpConst _ -> []
        op      -> opArgs op ++ concatMap dependencies (opArgs op)

constNegation :: Circuit -> Bool
constNegation c = not $ all refOk (M.keys (circ_refmap c))
  where
    refOk ref = case circ_refmap c ! ref of
        OpAdd _ _ -> True
        OpMul _ _ -> True
        OpSub x y | x /= (-1) -> True
                | otherwise -> notConst y
        OpInput _ -> True
        OpConst _ -> True

    notConst ref = case circ_refmap c ! ref of
        OpAdd x y -> notConst x || notConst y
        OpSub x y -> notConst x || notConst y
        OpMul x y -> notConst x || notConst y
        OpInput _ -> True
        OpConst _ -> False

intermediateGates :: Circuit -> [Ref]
intermediateGates c = filter intermediate (topologicalOrder c)
  where
    intermediate ref = notElem ref (circ_inputs c) &&
                       notElem ref (circ_consts c) &&
                       notElem ref (circ_outputs c)
