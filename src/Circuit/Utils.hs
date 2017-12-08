{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Circuit.Utils where

import Control.DeepSeq (deepseq)
import Control.Monad
import Control.Monad.Parallel
import Control.Monad.State.Strict
import Control.Parallel
import Control.Parallel.Strategies
import Crypto.Random
import Crypto.Random.DRBG
import Crypto.Util (bs2i)
import Data.Bits ((.&.))
import Data.Bits ((.&.), shift)
import Data.List.Split (chunksOf)
import GHC.Types
import System.IO
import qualified Data.ByteString as BS
import qualified Data.Map as M
import qualified GHC.Integer.GMP.Internals as GMP
import qualified GHC.Integer.GMP.Internals as GMP

import Prelude hiding (truncate)
sizeBase2 :: Integer -> Int
sizeBase2 x = fromIntegral (W# (GMP.sizeInBaseInteger x 2#))

invMod :: Integer -> Integer -> Integer
invMod = GMP.recipModInteger

mulMod :: Integer -> Integer -> Integer -> Integer
mulMod x y z = x * y `mod` z

addMod :: Integer -> Integer -> Integer -> Integer
addMod x y z = x + y `mod` z

sumMod :: [Integer] -> Integer -> Integer
sumMod xs q = foldl (\x y -> addMod x y q) 0 xs

prodMod :: [Integer] -> Integer -> Integer
prodMod xs q = foldl (\x y -> mulMod x y q) 1 xs

pmap :: NFData b => (a -> b) -> [a] -> [b]
pmap = parMap rdeepseq

plist :: NFData n => [n] -> [n]
plist = withStrategy (parList rdeepseq)

forceM :: (Monad m, NFData a) => a -> m ()
forceM x = x `deepseq` return ()

b2i :: Integral a => Bool -> a
b2i False = 0
b2i True  = 1

i2b :: Integral a => a -> Bool
i2b = not . (== 0)

pr :: String -> IO ()
pr s = do
    putStrLn s
    hFlush stdout

num2Base :: Int -> Int -> Integer -> [Integer]
num2Base base ndigits x = map fromIntegral $ reverse (map snd (take ndigits (tail ds)))
  where
    ds = (x, 0) : [ (div y (fromIntegral base), mod y (fromIntegral base)) | (y, _) <- ds ]

num2Bits :: Int -> Integer -> [Bool]
num2Bits ndigits x = reverse bs
  where
    bs = [ x .&. 2^i > 0 | i <- [0 .. ndigits-1] ]

bits2Num :: Integral a => [Bool] -> a
bits2Num bs = fromIntegral $ sum [ if b then shift 1 i :: Integer else 0 | b <- reverse bs | i <- [0..] ]

showInts :: [Integer] -> String
showInts = map toAlpha
  where
    alphas = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
    toAlpha n = alphas !! fromIntegral n

readInts :: String -> [Integer]
readInts = map fromAlpha
  where
    alphas = zip (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']) [0..]
    fromAlpha c = case lookup c alphas of
        Just n  -> n
        Nothing -> error ("[readInts] unkown character " ++ [c])

readBits :: String -> [Bool]
readBits s = if take 2 s == "0b"
                     then map (== '1') (drop 2 s)
                     else error "[readBitstring] unknown bitstring"

readBits' :: String -> [Bool]
readBits' = map (\c -> if c `notElem` "01"
                          then error ("[readBits'] unkonwn character " ++ [c])
                          else c == '1')

showBits :: [Bool] -> String
showBits bs = "0b" ++ showBits' bs

showBits' :: [Bool] -> String
showBits' = map (\b -> if b then '1' else '0')

red :: String -> String
red s = "\x1b[1;41m" ++ s ++ "\x1b[0m"

infixl 5 %
(%) :: Integral a => a -> a -> a
x % q = mod x q

safeInsert :: Ord a => String -> a -> b -> M.Map a b -> M.Map a b
safeInsert errorMsg x y m =
    if M.member x m
       then error errorMsg
       else M.insert x y m

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = []
combinations _ [] = []
combinations 1 xs = map (:[]) xs
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

permutations :: Int -> [a] -> [[a]]
permutations n possibleValues = Prelude.sequence (replicate n possibleValues)

-- all possible n-tuples of True, False
booleanPermutations :: Int -> [[Bool]]
booleanPermutations n = permutations n [False, True]

transpose :: [[a]] -> [[a]]
transpose xs | null (head xs) = []
             | otherwise = map head xs : transpose (map tail xs)


numBits :: (Integral a, Integral b) => a -> b
numBits n = ceiling (logBase 2 (fromIntegral n) :: Double)

foldTreeM :: Monad m => (a -> a -> m a) -> [a] -> m a
foldTreeM _ [ ] = error "[foldTreeM] empty list"
foldTreeM _ [x] = return x
foldTreeM f xs  = do
    let g ys = if length ys == 1
                  then return (head ys)
                  else f (ys!!0) (ys!!1)
    zs <- Prelude.mapM g (chunksOf 2 xs)
    foldTreeM f zs

xor :: Bool -> Bool -> Bool
xor False False = False
xor False True  = True
xor True  False = True
xor True  True  = True

--------------------------------------------------------------------------------
-- random generation

type Rng  = CtrDRBG
type Rand = State Rng

instance MonadParallel Rand where
    bindM2 f a b = do
        [r1, r2] <- splitRand 2
        let (x,_) = runRand a r1
            (y,_) = x `par` runRand b r2
        f x y

runRand :: Rand a -> Rng -> (a, Rng)
runRand = runState

evalRand :: Rand a -> Rng -> a
evalRand = evalState

randIO :: Rand a -> IO a
randIO m = do
    gen <- newGenIO
    let (x,_) = runRand m gen
    return x

randInteger_ :: Rng -> Int -> (Integer, Rng)
randInteger_ gen nbits = case genBytes nbytes gen of
    Left err    -> error ("[randInteger_] " ++ show err)
    Right (t,g) -> let i = bs2i (truncate t) in (i,g)
  where
    overflow = nbits `mod` 8
    nbytes   = ceiling ((fromIntegral nbits / 8) :: Double)

    truncate :: BS.ByteString -> BS.ByteString
    truncate bs = BS.cons w'' (BS.tail bs)
        where
            w   = BS.head bs
            w'  = w .&. (2^(nbits `mod` 8) - 1)
            w'' = if overflow == 0 then w else w'

randInteger :: Int -> Rand Integer
randInteger nbits = do
    rng <- get
    let (x, rng') = randInteger_ rng nbits
    put rng'
    return x

randBits :: Int -> Rand [Bool]
randBits n = num2Bits n <$> randInteger n

randBitsIO :: Int -> IO [Bool]
randBitsIO n = randIO (randBits n)

randKeyIO :: Int -> IO [Integer]
randKeyIO n = map b2i <$> randBitsIO n

randIntegerMod :: Integer -> Rand Integer
randIntegerMod q = do
    let nbits = sizeBase2 q
    x <- randInteger nbits
    if x <= 0 || x >= q then
        randIntegerMod q
    else
        return x

randIntegerModIO :: Integer -> IO Integer
randIntegerModIO q = randIO (randIntegerMod q)

randIntMod :: Int -> Rand Int
randIntMod q = fromIntegral <$> randIntegerMod (fromIntegral q)

randIntModIO :: Int -> IO Int
randIntModIO q = randIO (randIntMod q)

randPrimes :: Int -> Int -> Rand [Integer]
randPrimes nprimes nbits = do
    rngs <- splitRand nprimes
    let ps = pmap (GMP.nextPrimeInteger . fst . flip randInteger_ nbits) rngs
    return ps

randInv :: Integer -> Rand (Integer, Integer)
randInv q = try (100 :: Int)
  where
    try 0 = error "[randInv] ran out of tries!"
    try n = do
        x <- randIntegerMod q
        let xinv = invMod x q
        if x == 0 || xinv == 0
            then try (n-1)
            else return (x, xinv)

randInvs :: Int -> Integer -> Rand [(Integer, Integer)]
randInvs ninvs modulus = do
    rngs <- splitRand ninvs
    let invs = pmap fst (map (runRand (randInv modulus)) rngs)
    return invs

splitRand :: Int -> Rand [Rng]
splitRand 0 = return []
splitRand n = do
    gen <- get
    case splitGen gen of
        Left err      -> error ("[splitGen] " ++ show err)
        Right (g0,g1) -> do
            put g0
            rest <- splitRand (n-1)
            return (g1:rest)

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f (x:xs) = foldM f x xs
