{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Circuit where

import Util
import Util (forceM, pmap)

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

type Ref = Int
type ID  = Int
type Val = Int

data Op = Add Ref Ref
        | Sub Ref Ref
        | Mul Ref Ref
        | Const ID
        | Input ID
        deriving (Eq, Ord, Show)

data Circuit = Circuit {
      circ_outrefs :: [Ref]
    , circ_inprefs :: M.Map ID Ref -- TODO: maybe dont need these maps
    , circ_refmap  :: M.Map Ref Op
    , circ_consts  :: [Integer]
    } deriving (Show)

emptyCirc = Circuit [] M.empty M.empty []

type TestCase = ([Bool], [Bool])

opArgs :: Op -> [Ref]
opArgs (Add x y) = [x,y]
{-opArgs (Sub 0 x) = [x]-}
opArgs (Sub x y) = [x,y]
opArgs (Mul x y) = [x,y]
opArgs (Const _) = []
opArgs (Input _) = []

ninputs :: Circuit -> Int
ninputs = M.size . circ_inprefs

nconsts :: Circuit -> Int
nconsts = length . circ_consts

ydeg :: Circuit -> Int
ydeg c = varDegree c (Const (-1))

xdeg :: Circuit -> Int -> Int
xdeg c i = xdegs c !! i

xdegs :: Circuit -> [Int]
xdegs c = pmap (varDegree c . Input) [0 .. ninputs c - 1]

depth :: Circuit -> Int
depth c = maximum $ foldCirc f c
  where
    f (Input _) []  = 0
    f (Const _) []  = 0
    f _         xs  = maximum xs + 1

-- i think it makes sense that since these are just polynomials in a single
-- variable then finding the degree is this easy
varDegree :: Circuit -> Op -> Int
varDegree c z = maximum $ foldCirc f c
  where
    f (Add _ _) [x,y] = max x y
    f (Sub _ _) [x,y] = max x y
    f (Mul _ _) [x,y] = x + y
    f x _ = if eq x z then 1 else 0

    eq (Input x) (Input y) = x == y
    eq (Const _) (Const _) = True
    eq _         _         = False

-- TODO make me better! does this really reflect the degree of the multivariate
-- polynomial corresponding to C?
circDegree :: Circuit -> Int
circDegree c = maximum $ foldCirc f c
  where
    f (Add _ _) [x,y] = max x y
    f (Sub _ _) [x,y] = max x y
    f (Mul _ _) [x,y] = x + y
    f (Input id) _ = 1
    f (Const id) _ = 1

-- note: inputs are little endian: [x0, x1, ..., xn]
evalMod :: (Show a, Integral a) => Circuit -> [a] -> [a] -> a -> [a]
evalMod c xs ys q = foldCirc eval c
  where
    eval (Add _ _) [x,y] = x + y % q
    {-eval (Sub 0 _) [x]   = 1 - x % q-}
    eval (Sub _ _) [x,y] = x - y % q
    eval (Mul _ _) [x,y] = x * y % q
    eval (Input i) []    = xs !! i
    eval (Const i) []    = ys !! i
    eval op        args  = error ("[evalMod] weird input: " ++ show op ++ " " ++ show args)

-- note: inputs are little endian: [x0, x1, ..., xn]
plainEval :: Circuit -> [Bool] -> [Bool]
plainEval c xs = map (/= 0) (foldCirc eval c)
  where
    eval :: Op -> [Integer] -> Integer
    eval (Add _ _) [x,y] = x + y
    {-eval (Sub 0 _) [x]   = 1 - x-}
    eval (Sub _ _) [x,y] = x - y
    eval (Mul _ _) [x,y] = x * y
    eval (Input i) []    = b2i (xs !! i)
    eval (Const i) []    = fromIntegral (circ_consts c !! i)
    eval op        args  = error ("[plainEval] weird input: " ++ show op ++ " " ++ show args)

-- note: inputs are little endian: [x0, x1, ..., xn]
plainEvalIO :: Circuit -> [Bool] -> IO [Bool]
plainEvalIO c xs = do
    zs <- foldCircIO eval c
    return $ map (/= 0) zs
  where
    eval :: Op -> [Integer] -> Integer
    eval (Add _ _) [x,y] = x + y
    {-eval (Sub 0 _) [x]   = 1 - x-}
    eval (Sub _ _) [x,y] = x - y
    eval (Mul _ _) [x,y] = x * y
    eval (Input i) []    = b2i (xs !! i)
    eval (Const i) []    = fromIntegral (circ_consts c !! i)
    eval op        args  = error ("[plainEval] weird input: " ++ show op ++ " " ++ show args)

ensure :: Bool -> (Circuit -> [Bool] -> IO [Bool]) -> Circuit -> [TestCase] -> IO Bool
ensure verbose eval c ts = and <$> mapM ensure' (zip [(0::Int)..] ts)
  where
    toBit :: Bool -> Char
    toBit b = if b then '1' else '0'

    toBits :: [Bool] -> String
    toBits = map toBit

    ensure' (i, (inps, outs)) = do
        res <- eval c (reverse inps)
        if res == outs then do
            let s = printf "test %d succeeded: input:%s expected:%s got:%s"
                            i (toBits inps) (toBits outs) (toBits res)
            when verbose (putStrLn s)
            return True
        else do
            let s = printf "test %d failed! input:%s expected:%s got:%s"
                            i (toBits inps) (toBits outs) (toBits res)
            putStrLn (red s)
            return False

foldCirc :: (Op -> [a] -> a) -> Circuit -> [a]
foldCirc f c = runIdentity (foldCircM f' c)
  where
    f' op _ xs = return (f op xs)

foldCircM :: Monad m => (Op -> Ref -> [a] -> m a) -> Circuit -> m [a]
foldCircM f c = evalStateT (mapM eval (circ_outrefs c)) M.empty
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
    mapM (atomically . readTMVar . (mem !)) (circ_outrefs c)

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
        Input _ -> []
        Const _ -> []
        op      -> opArgs op ++ concatMap dependencies (opArgs op)

constNegation :: Circuit -> Bool
constNegation c = not $ all refOk (M.keys (circ_refmap c))
  where
    refOk ref = case circ_refmap c ! ref of
        Add _ _ -> True
        Mul _ _ -> True
        Sub x y | x /= (-1) -> True
                | otherwise -> notConst y
        Input _ -> True
        Const _ -> True

    notConst ref = case circ_refmap c ! ref of
        Add x y -> notConst x || notConst y
        Sub x y -> notConst x || notConst y
        Mul x y -> notConst x || notConst y
        Input _ -> True
        Const _ -> False
