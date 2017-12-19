{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE Strict #-}
#endif

module Circuit where

import Circuit.Utils

import Control.Monad.Identity
import Control.Monad.State.Strict
import Data.List (nub, sortBy)
import Lens.Micro.Platform
import Text.Printf
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

newtype Ref = Ref { getRef :: Int } deriving (Eq, Ord, Num)
newtype Id  = Id  { getId  :: Int } deriving (Eq, Ord, Num)

instance Show Ref where
    show ref = show (getRef ref)

instance Show Id where
    show id = show (getId id)

data ArithGate =
      ArithAdd !Ref !Ref
    | ArithSub !Ref !Ref
    | ArithMul !Ref !Ref
    | ArithInput !Id
    | ArithConst !Id
    deriving (Eq, Ord, Show)

data BoolGate =
      BoolXor !Ref !Ref
    | BoolAnd !Ref !Ref
    | BoolNot !Ref
    | BoolInput !Id
    | BoolConst !Id
    deriving (Eq, Ord, Show)

data Circuit gate = Circuit
    { _circ_outputs     :: ![Ref]
    , _circ_inputs      :: ![Ref]
    , _circ_consts      :: !(M.Map Ref Id)
    , _circ_secret_refs :: !(IS.IntSet)
    , _circ_secret_ids  :: !(IS.IntSet)
    , _circ_refmap      :: !(IM.IntMap gate)
    , _circ_const_vals  :: !(M.Map Id Integer)
    , _circ_symlen      :: !Int
    , _circ_base        :: !Integer
    } deriving (Show)

makeLenses ''Circuit

type TestCase = ([Integer], [Integer])

type Acirc = Circuit ArithGate
type Circ = Circuit BoolGate

---------------------------------------------------------------------------------------
-- GateEval class allows us to share boilerplate between binary and arithmetic circuits

class (Eq g, Ord g) => GateEval g where
    gateArgs :: g -> [Ref]
    gateEval :: (Id -> Integer) -> (Id -> Integer) -> g -> [Integer] -> Integer
    gateAdd :: Ref -> Ref -> g
    gateSub :: Ref -> Ref -> g
    gateMul :: Ref -> Ref -> g
    gateXor :: Ref -> Ref -> Maybe g
    gateNot :: Ref -> Maybe g
    gateInput :: Id -> g
    gateConst :: Id -> g
    gateIsMul :: g -> Bool
    gateIsGate :: g -> Bool

instance GateEval ArithGate where
    gateArgs (ArithAdd x y)  = [x,y]
    gateArgs (ArithSub x y)  = [x,y]
    gateArgs (ArithMul x y)  = [x,y]
    gateArgs (ArithInput _) = []
    gateArgs (ArithConst _) = []

    gateEval _ _ (ArithAdd _ _) [x,y] = x + y
    gateEval _ _ (ArithSub _ _) [x,y] = x - y
    gateEval _ _ (ArithMul _ _) [x,y] = x * y
    gateEval getInp _   (ArithInput i) [] = getInp i
    gateEval _ getConst (ArithConst i) [] = getConst i

    gateAdd x y = ArithAdd x y
    gateSub x y = ArithSub x y
    gateMul x y = ArithMul x y
    gateXor _ _ = Nothing
    gateNot _   = Nothing
    gateInput i = ArithInput i
    gateConst i = ArithConst i

    gateIsMul (ArithMul _ _) = True
    gateIsMul _ = False

    gateIsGate (ArithInput _) = False
    gateIsGate (ArithConst _) = False
    gateIsGate _ = True

instance GateEval BoolGate where
    gateArgs (BoolXor x y) = [x,y]
    gateArgs (BoolAnd x y) = [x,y]
    gateArgs (BoolNot x)   = [x]
    gateArgs (BoolInput _) = []
    gateArgs (BoolConst _) = []

    gateEval _ _ (BoolXor _ _) [x,y] = b2i (i2b x `xor` i2b y)
    gateEval _ _ (BoolAnd _ _) [x,y] = x * y
    gateEval _ _ (BoolNot _)   [x]   = 1 - x
    gateEval getInp _   (BoolInput i) [] = getInp i
    gateEval _ getConst (BoolConst i) [] = getConst i

    gateAdd x y = BoolXor x y
    gateSub x y = BoolXor x y
    gateMul x y = BoolAnd x y
    gateXor x y = Just (BoolXor x y)
    gateNot x   = Just (BoolNot x)
    gateInput i = BoolInput i
    gateConst i = BoolConst i

    gateIsMul (BoolAnd _ _) = True
    gateIsMul _ = False

    gateIsGate (BoolInput _) = False
    gateIsGate (BoolConst _) = False
    gateIsGate _ = True

--------------------------------------------------------------------------------
-- Generic circuit functions

emptyCirc :: Circuit a
emptyCirc = Circuit [] [] M.empty IS.empty IS.empty IM.empty M.empty 1 2

getConst :: Circuit gate -> Id -> Integer
getConst c id = case c^.circ_const_vals.at id of
    Just x  -> x
    Nothing -> error ("[getConst] no const known for y" ++ show id)

secretConst :: Circuit gate -> Id -> Bool
secretConst c id = IS.member (getId id) (_circ_secret_ids c)

secretRefs :: Circuit gate -> [Ref]
secretRefs c = map Ref $ IS.toAscList (_circ_secret_refs c)

getGate :: Circuit gate -> Ref -> gate
getGate c ref = case c^.circ_refmap.at (getRef ref) of
    Nothing   -> error (printf "[getGate] no ref %d!" (getRef ref))
    Just gate -> gate

randomizeSecrets :: Circuit gate -> IO (Circuit gate)
randomizeSecrets c = do
    key <- replicateM (nsecrets c) $ randIntegerModIO (fromIntegral (_circ_base c))
    let newSecrets = M.fromList $ zip (map Id $ IS.toAscList (c^.circ_secret_ids)) key
    return $ c & circ_const_vals %~ M.union newSecrets

genTest :: GateEval gate => Circuit gate -> IO TestCase
genTest c
    | c^.circ_symlen == 1 = do
        let q = _circ_base c ^ (fromIntegral (ninputs c) :: Integer)
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


printCircInfo :: GateEval g => Circuit g -> IO ()
printCircInfo c = do
    let n = ninputs c
    printf "circuit info\n"
    printf "============\n"
    printf "ninputs=%d noutputs=%d nconsts=%d nsecrets=%d\n" n (noutputs c) (nconsts c) (nsecrets c)
    printf "symlen=%d base=%d\n" (symlen c) (c^.circ_base)
    printf "ngates=%d depth=%d\n" (ngates c) (depth c)
    printf "degree=%d\n" (circDegree c)

printTruthTable :: GateEval gate => Circuit gate -> IO ()
printTruthTable c = forM_ inputs $ \inp -> do
    let out = plainEval c inp
    printf "%s -> %s\n" (showInts inp) (showInts out)
  where
    n = ninputs c `div` symlen c
    sym x = [ if i == x then (1 :: Integer) else 0 | i <- [ 0 .. symlen c - 1 ] ]
    inputs = case symlen c of
        1 -> sequence (replicate (ninputs c) [(0::Integer)..fromIntegral (c^.circ_base - 1)])
        _ -> map concat $ sequence (replicate n (map sym [0..symlen c - 1]))

circEq :: GateEval gate => Circuit gate -> Circuit gate -> IO Bool
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

ngates :: Circuit gate -> Int
ngates = IM.size . _circ_refmap

ninputs :: Circuit gate -> Int
ninputs = length . _circ_inputs

nconsts :: Circuit gate -> Int
nconsts = length . _circ_consts

nsecrets :: Circuit gate -> Int
nsecrets = IS.size . _circ_secret_refs

noutputs :: Circuit gate -> Int
noutputs = length . _circ_outputs

symlen :: Circuit gate -> Int
symlen = _circ_symlen

ydeg :: Circuit ArithGate -> Integer
ydeg c = head (degs c)

xdeg :: Circuit ArithGate -> Int -> Integer
xdeg c i = degs c !! (i+1)

degs :: Circuit ArithGate -> [Integer]
degs c = map (varDegree c) ids
  where
    ids = ArithConst (Id (-1)) : map (ArithInput . Id) [0 .. ninputs c-1]

depth :: GateEval gate => Circuit gate -> Integer
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

circDegree :: GateEval g => Circuit g -> Integer
circDegree c = maximum $ foldCirc f c
  where
    f g [x,y] = if gateIsMul g then x + y else max x y
    f g [x] = x
    f _ [] = 1 -- input or const

zeroTest :: Integer -> Integer
zeroTest 0 = 0
zeroTest _ = 1

plainEval :: GateEval gate => Circuit gate -> [Integer] -> [Integer]
plainEval c inps
    | ninputs c /= length inps =
        error (printf "[plainEval] incorrect number of inputs: expected %d, got %s" (ninputs c) (show inps))
    | otherwise = map zeroTest $ foldCirc (gateEval getInp (getConst c)) c
  where
    getInp i = inps !! getId i

ensure :: GateEval gate => Bool -> Circuit gate -> [TestCase] -> IO Bool
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


ensureIO :: Bool -> (Circuit gate -> [Integer] -> IO [Integer]) -> Circuit gate -> [TestCase] -> IO Bool
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

foldCirc :: GateEval gate => (gate -> [a] -> a) -> Circuit gate -> [a]
foldCirc f c = runIdentity (foldCircM f' c)
  where
    f' op _ xs = return (f op xs)

foldCircRef :: GateEval gate => (gate -> Ref -> [a] -> a) -> Circuit gate -> [a]
foldCircRef f c = runIdentity (foldCircM f' c)
  where
    f' op ref xs = return (f op ref xs)

-- evaluate the circuit
foldCircM :: (GateEval g, Monad m) => (g -> Ref -> [a] -> m a) -> Circuit g -> m [a]
foldCircM f c = evalStateT (mapM (foldCircRec f c) (c^.circ_outputs)) M.empty

-- evaluate the circuit for a particular ref as output
foldCircRefM :: (GateEval g, Monad m) => (g -> Ref -> [a] -> m a) -> Circuit g -> Ref -> m a
foldCircRefM !f !c !ref = evalStateT (foldCircRec f c ref) M.empty

-- helper function for foldCircM and foldCircRefM
foldCircRec :: (GateEval g, Monad m)
            => (g -> Ref -> [a] -> m a) -> Circuit g -> Ref
            -> StateT (M.Map Ref a) m a
foldCircRec f c !ref = do
    existingVal <- gets (M.lookup ref)
    case existingVal of
        Just !val -> return val -- evaluated already
        Nothing   -> do
            -- get the gate
            let !g = c^.circ_refmap.at (getRef ref).non
                    (error $ printf "[foldCircMRec] no gate for ref %s!" (show ref))
            argVals <- mapM (foldCircRec f c) (gateArgs g)
            val     <- lift (f g ref argVals)
            modify' (M.insert ref val)
            return val

topologicalOrder :: GateEval gate => Circuit gate -> [Ref]
topologicalOrder c = reverse $ execState (foldCircM eval c) []
  where
    eval :: gate -> Ref -> [a] -> State [Ref] ()
    eval _ ref _ = modify (ref:)

-- Ugly and fast!
topoLevels :: GateEval gate => Circuit gate -> [[Ref]]
topoLevels c = nub $ map snd $ M.toAscList $ execState (foldCircM eval c) M.empty
  where
    eval :: gate -> Ref -> [Int] -> State (M.Map Int [Ref]) Int
    eval _ ref [] = modify (M.insertWith (++) 0 [ref]) >> return 0
    eval _ ref ds = do
        let d = 1 + maximum ds
        modify (M.insertWith (++) d [ref])
        return d

sortGates :: GateEval gate => Circuit gate -> [Ref]
sortGates c = concatMap (sortBy refDist) (topoLevels c)
  where
    refDist xref yref = let xargs = gateArgs (getGate c xref)
                            yargs = gateArgs (getGate c yref)
                        in case (xargs, yargs) of
                            ([], []) -> EQ
                            (_,  []) -> GT
                            ([], _ ) -> LT
                            (xs, ys) -> let cs = zipWith compare xs ys
                                        in if any (== EQ) cs then EQ else maximum cs

gates :: GateEval gate => Circuit gate -> [(Ref, gate)]
gates c = filter (f.snd) $ map (\ref -> (ref, getGate c ref)) (topologicalOrder c)
  where
    f gate = not $ null (gateArgs gate)

gateRefs :: GateEval gate => Circuit gate -> [Ref]
gateRefs = map fst . gates

nonInputGateRefs :: GateEval gate => Circuit gate -> [Ref]
nonInputGateRefs = map fst . filter (gateIsGate.snd) . gates

sortedNonInputGates :: GateEval gate => Circuit gate -> [Ref]
sortedNonInputGates c = filter notInput (sortGates c)
  where
    notInput ref = notElem ref (_circ_inputs c) &&
                   IS.notMember (getRef ref) (_circ_secret_refs c)

intermediateGates :: GateEval gate => Circuit gate -> [Ref]
intermediateGates c = filter intermediate (topologicalOrder c)
  where
    intermediate ref = notElem ref (_circ_inputs c) &&
                       notElem ref (_circ_outputs c) &&
                       IS.notMember (getRef ref) (_circ_secret_refs c)
