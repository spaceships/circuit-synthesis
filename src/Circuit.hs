{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE Strict #-}
#endif

module Circuit where

import Util
import Rand

import Control.Monad.Identity
import Control.Monad.Par (IVar, Par, fork, runPar)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.ParallelIO
import Control.DeepSeq (NFData)
import Control.Monad.IfElse (whenM)
import Control.Monad.State.Strict
import Data.Map.Strict ((!))
import Text.Printf
import qualified Control.Monad.Par as IVar
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
    , circ_secrets     :: M.Map Id Int
    , circ_secret_refs :: M.Map Ref Id
    , circ_refmap      :: M.Map Ref Op
    , circ_consts      :: B.Bimap Int Ref
    , circ_const_ids   :: S.Set Id -- which OpSecrets are public
    , circ_symlen      :: Int
    , circ_base        :: Int -- the expected base of the inputs
    } deriving (Show)

type TestCase = ([Int], [Int])

--------------------------------------------------------------------------------
-- instances and such

emptyCirc :: Circuit
emptyCirc = Circuit [] [] M.empty M.empty M.empty B.empty S.empty 1 2

instance Show Ref where
    show ref = show (getRef ref)

instance Show Id where
    show id = show (getId id)

getSecret :: Circuit -> Id -> Int
getSecret c id = case M.lookup id (circ_secrets c) of
    Just x  -> x
    Nothing -> error ("[getSecret] no secret known for y" ++ show id)

publicConst :: Circuit -> Id -> Bool
publicConst c id = S.member id (circ_const_ids c)

-- refs of all non-public consts
secretRefs :: Circuit -> [Ref]
secretRefs c = map fst $ filter (not . publicConst c . snd) $ M.toList (circ_secret_refs c)

getGate :: Circuit -> Ref -> Op
getGate c ref = case M.lookup ref (circ_refmap c) of
    Nothing -> error (printf "[getGate] no ref %d!" (getRef ref))
    Just op -> op

randomizeSecrets :: Circuit -> IO Circuit
randomizeSecrets c = do
    key <- map b2i <$> num2Bits (nsecrets c) <$> randIO (randInteger (nsecrets c))
    return $ flip execState c $ do
        forM [0..nsecrets c-1] $ \i -> do
            modify $ \c -> c { circ_secrets = M.insert (Id i) (key !! i) (circ_secrets c) }

-- symlen determines how large a rachel symbol is
genTest :: Circuit -> IO TestCase
genTest c
    | circ_symlen c == 1 = do
        let q = (fromIntegral (circ_base c) :: Integer) ^ (fromIntegral (ninputs c) :: Integer)
        inp <- num2Base (circ_base c) (ninputs c) <$> randIO (randIntegerMod q)
        return (inp, plainEval c inp)
    | otherwise = do
        when ((ninputs c `mod` circ_symlen c) /= 0) $
            error "[genTest] inputs not evenly dividable"
        let nsyms = ninputs c `div` circ_symlen c
        inp <- fmap concat $ replicateM nsyms $ do
            x <- fromIntegral <$> randIO (randInteger (numBits (circ_symlen c)))
            return [ if i == x then 1 else 0 | i <- [0..circ_symlen c-1] ]
        return (inp, plainEval c inp)


printCircInfo :: Circuit -> IO ()
printCircInfo c = do
    -- let ds = degs c
    let n = ninputs c
    printf "circuit info\n"
    printf "============\n"
    printf "ninputs=%d noutputs=%d nconsts=%d symlen=%d base=%d\n"
            n (noutputs c) (nconsts c) (symlen c) (circ_base c)
    -- printf "ngates=%d depth=%d var-degree=%d circ-degree=%d\n"
    --         (ngates c) (depth c) (sum ds) (circDegree c)
    printf "ngates=%d depth=%d circ-degree=%d\n"
            (ngates c) (depth c) (circDegree c)
    -- printf "number of additions with disjoint indices: %d\n"
            -- (numDisjointAdditions c)

printCircInfoLatex :: Circuit -> IO ()
printCircInfoLatex c = do
    let ds = degs c
        n = fromIntegral $ ninputs c
    printf "#1 & #2 & %d & %d & %d & %d & #3 & %d & %d & %d & #4\\\\ \\hline\n"
            n (nconsts c) (noutputs c) (ngates c) (depth c) (sum ds) (sum ds + 2*n)

printSecrets :: Circuit -> IO ()
printSecrets c = do
    mapM (putStr . show) (M.elems (circ_secrets c))
    putStrLn ""

printTruthTable :: Circuit -> IO ()
printTruthTable c = forM_ inputs $ \inp -> do
    let out = plainEval c inp
    printf "%s -> %s\n" (showInts inp) (showInts out)

  where
    n = ninputs c `div` symlen c
    sym x = [ if i == x then 1 else 0 | i <- [ 0 .. symlen c - 1 ] ]
    inputs = case symlen c of
        1 -> sequence (replicate (ninputs c) [0..circ_base c - 1])
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

symlen :: Circuit -> Int
symlen = circ_symlen

ydeg :: Circuit -> Integer
ydeg c = head (degs c)

xdeg :: Circuit -> Int -> Integer
xdeg c i = degs c !! (i+1)

degs :: Circuit -> [Integer]
degs c = map (varDegree c) ids
  where
    ids = OpSecret (Id (-1)) : map (OpInput . Id) [0 .. ninputs c-1]

depth :: Circuit -> Integer
depth c = maximum $ foldCirc f c
  where
    f (OpInput  _) [] = 0
    f (OpSecret _) [] = 0
    f _            xs = maximum xs + 1

varDegree :: Circuit -> Op -> Integer
varDegree c z = maximum (varDegree' c z)

varDegree' :: Circuit -> Op -> [Integer]
varDegree' c z = foldCirc f c
  where
    f (OpAdd _ _) [x,y] = max x y
    f (OpSub _ _) [x,y] = max x y
    f (OpMul _ _) [x,y] = x + y

    f x _ = if eq x z then 1 else 0

    eq (OpInput  x) (OpInput  y) = x == y
    eq (OpSecret _) (OpSecret _) = True
    eq _ _ = False

-- TODO make me better! does this really reflect the degree of the multivariate
-- polynomial corresponding to C?
circDegree :: Circuit -> Integer
circDegree c = maximum $ foldCirc f c
  where
    f (OpAdd _ _) [x,y] = max x y
    f (OpSub _ _) [x,y] = max x y
    f (OpMul _ _) [x,y] = x + y
    f (OpInput  _) _ = 1
    f (OpSecret _) _ = 1
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

zeroTest :: Int -> Int
zeroTest 0 = 0
zeroTest _ = 1

plainEval :: Circuit -> [Int] -> [Int]
plainEval c inps
    | ninputs c /= length inps =
        error (printf "[plainEval] incorrect number of inputs: expected %d, got %s" (ninputs c) (show inps))
    | otherwise = map zeroTest $ foldCirc eval c
  where
    eval :: Op -> [Int] -> Int
    eval (OpAdd _ _) [x,y] = x + y
    eval (OpSub _ _) [x,y] = x - y
    eval (OpMul _ _) [x,y] = x * y
    eval (OpInput  i) [] = inps !! getId i
    eval (OpSecret i) [] = getSecret c i
    eval op args = error ("[plainEval] weird input: " ++ show op ++ " " ++ show args)

parEval :: Circuit -> [Int] -> [Int]
parEval c inps = map zeroTest $ runPar $ mapM IVar.get =<< foldCircM eval c
  where
    eval :: Op -> Ref -> [IVar Int] -> Par (IVar Int)
    eval (OpAdd _ _) _ [x,y] = liftBin (+) x y
    eval (OpSub _ _) _ [x,y] = liftBin (-) x y
    eval (OpMul _ _) _ [x,y] = liftBin (*) x y
    eval (OpInput  i) _ [] = IVar.newFull (inps !! getId i)
    eval (OpSecret i) _ [] = IVar.newFull (getSecret c i)
    eval op _ args = error ("[parEval] weird input: " ++ show op ++ " with " ++ show (length args) ++ " arguments")

    liftBin f x y = do
        result <- IVar.new
        fork (liftM2 f (IVar.get x) (IVar.get y) >>= IVar.put result)
        return result

plainEvalIO :: Circuit -> [Int] -> IO [Int]
plainEvalIO c xs = map zeroTest <$> foldCircIO eval c
  where
    eval :: Op -> [Int] -> Int
    eval (OpAdd _ _) [x,y] = x + y
    eval (OpSub _ _) [x,y] = x - y
    eval (OpMul _ _) [x,y] = x * y
    eval (OpInput  i) [] = xs !! getId i
    eval (OpSecret i) [] = getSecret c i
    eval op args = error ("[plainEval] weird input: " ++ show op ++ " " ++ show args)

ensure :: Bool -> Circuit -> [TestCase] -> IO Bool
ensure verbose c ts = and <$> mapM ensure' (zip [(0::Int)..] ts)
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


ensureIO :: Bool -> (Circuit -> [Int] -> IO [Int]) -> Circuit -> [TestCase] -> IO Bool
ensureIO verbose eval c ts = and <$> mapM ensureIO' (zip [(0::Int)..] ts)
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

foldCircM :: Monad m => (Op -> Ref -> [a] -> m a) -> Circuit -> m [a]
foldCircM f c = evalStateT (mapM eval (circ_outputs c)) M.empty
  where
    eval ref = gets (M.lookup ref) >>= \case
        Just val -> return val
        Nothing  -> do
            when (M.notMember ref (circ_refmap c))
                (traceM (printf "unknown ref \"%s\"" (show ref)))
            let op = circ_refmap c ! ref
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
            when (M.notMember ref (circ_refmap c))
                (traceM (printf "unknown ref \"%s\"" (show ref)))
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

nonInputGates :: Circuit -> [Ref]
nonInputGates c = filter notInput (topologicalOrder c)
  where
    notInput ref = notElem ref (circ_inputs c) &&
                   M.notMember ref (circ_secret_refs c)

intermediateGates :: Circuit -> [Ref]
intermediateGates c = filter intermediate (topologicalOrder c)
  where
    intermediate ref = notElem ref (circ_inputs c) &&
                       notElem ref (circ_outputs c) &&
                       M.notMember ref (circ_secret_refs c)

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
    eval op@(OpSecret id) _ _ = if publicConst c id
                                   then return M.empty
                                   else return (M.singleton op 1)
    eval _ _ _ = undefined

