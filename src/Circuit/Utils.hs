{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Circuit.Utils where

import Control.Monad
import Control.Monad.State.Strict
import Crypto.Random (newGenIO, genBytes, splitGen)
import Crypto.Random.DRBG (CtrDRBG)
import Crypto.Util (bs2i)
import Data.Bits ((.&.), shift, Bits)
import Data.Binary.Get (runGet, getWord64host)
import Text.Printf
import GHC.Types
import System.IO
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
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

b2i :: Integral a => Bool -> a
b2i False = 0
b2i True  = 1

i2b :: Integral a => a -> Bool
i2b = not . (== 0)

pr :: String -> IO ()
pr s = do
    putStrLn s
    hFlush stdout

num2Base :: Integer -> Int -> Integer -> [Int]
num2Base base ndigits x = map fromIntegral $ reverse (map snd (take ndigits (tail ds)))
  where
    ds = (x, 0) : [ (div y (fromIntegral base), mod y (fromIntegral base)) | (y, _) <- ds ]

num2Bits :: (Bits a, Ord a, Num a) => Int -> a -> [Bool]
num2Bits ndigits x = reverse bs
  where
    bs = [ x .&. 2^i > 0 | i <- [0 .. ndigits-1] ]

str2Bits :: String -> [Bool]
str2Bits = concatMap (num2Bits 8 . fromEnum)

bits2Num :: Integral a => [Bool] -> a
bits2Num bs = fromIntegral $ sum [ if b then shift 1 i :: Integer else 0 | b <- reverse bs | i <- [0..] ]

showInts :: [Int] -> String
showInts = map toAlpha
  where
    alphas = ['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']
    toAlpha n = alphas !! n

readInts :: String -> [Int]
readInts = map fromAlpha
  where
    alphas = zip (['0'..'9'] ++ ['a'..'z'] ++ ['A'..'Z']) [0..]
    fromAlpha c = case lookup c alphas of
        Just n  -> n
        Nothing -> error ("[readInts] unkown character " ++ [c])

readBits :: String -> [Bool]
readBits s = if take 2 s == "0b"
                     then map (== '1') (drop 2 s)
                     else error "[readBits] unknown bitstring"

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

-- infixl 5 %
-- (%) :: Integral a => a -> a -> a
-- x % q = mod x q

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
permutations n xs = Prelude.sequence (replicate n xs)

-- all possible n-tuples of True, False
booleanPermutations :: Int -> [[Bool]]
booleanPermutations n = permutations n [False, True]

bitPermutations :: Int -> [[Int]]
bitPermutations n = permutations n [0,1]

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
xor True  True  = False

xorInt :: Int -> Int -> Int
xorInt x y = b2i (i2b x `xor` i2b y)

--------------------------------------------------------------------------------
-- random generation

type Rng  = CtrDRBG
type Rand = State Rng

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

randInt_ :: Rng -> (Int, Rng)
randInt_ gen = case genBytes 16 gen of
    Left err -> error ("[randInt_] " ++ show err)
    Right (bs,g) -> (fromIntegral (runGet getWord64host (BL.fromStrict bs)), g)

randInts_ :: Int -> Rng -> ([Int], Rng)
randInts_ n g = case genBytes (16*n) g of
    Left err -> error ("[randInt_] " ++ show err)
    Right (bs,g') ->
        let words = flip runGet (BL.fromStrict bs) $ do
                Control.Monad.replicateM n getWord64host
        in (map fromIntegral words, g')

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

randKeyIO :: Int -> IO [Int]
randKeyIO n = randIO $ map b2i <$> randBits n

randIntegerMod :: Integer -> Rand Integer
randIntegerMod q = do
    let nbits = sizeBase2 q
    x <- randInteger nbits
    if x >= q then
        randIntegerMod q
    else
        return x

randIntegerModIO :: Integer -> IO Integer
randIntegerModIO q = randIO (randIntegerMod q)

randInt :: Rand Int
randInt = do
    rng <- get
    let (!x, !rng') = randInt_ rng
    put rng'
    return x

randInts :: Int -> Rand [Int]
randInts n = do
    rng <- get
    let (!xs, !rng') = randInts_ n rng
    put rng'
    return xs

randIntsMod :: Int -> Int -> Rand [Int]
randIntsMod n q = map (flip mod q) <$> randInts n

randIntMod :: Int -> Rand Int
randIntMod q = flip mod q <$> randInt

randIntModIO :: Int -> IO Int
randIntModIO q = randIO (randIntMod q)

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

-- randomly permute the list using a knuth shuffle
randomize :: [a] -> Rand [a]
randomize []  = return []
randomize [x] = return [x]
randomize xs  = do
    n <- randIntMod (length xs)
    let (z:ys) = swap n xs
    zs <- randomize ys
    return (z:zs)
  where
    swap 0 xs = xs
    swap i (x:xs) = let y = xs !! (i-1) in y : take (i-1) xs ++ [x] ++ drop i xs

--------------------------------------------------------------------------------
-- other helpers

foldM1 :: Monad m => (a -> a -> m a) -> [a] -> m a
foldM1 f (x:xs) = foldM f x xs

pairsOf :: [a] -> [(a,a)]
pairsOf [] = []
pairsOf (x:y:xs) = (x,y) : pairsOf xs
pairsOf _ = error "[pairsOf] odd number of inputs!"

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

safeChunksOf :: Int -> [a] -> [[a]]
safeChunksOf _ [] = []
safeChunksOf n xs = safeTake n xs : safeChunksOf n (drop n xs)
  where
    safeTake 0 xs = []
    safeTake n (x:xs) = x : safeTake (n-1) xs
    safeTake n [] = error $ printf "[safeChunksOf] not enough elements: expected %d!" n

chunksOfPad :: Int -> a -> [a] -> [[a]]
chunksOfPad _ _   [] = []
chunksOfPad n pad xs = padTake n xs : chunksOfPad n pad (drop n xs)
  where
    padTake 0 xs     = []
    padTake n (x:xs) = x   : padTake (n-1) xs
    padTake n []     = pad : padTake (n-1) []

sigmaVector :: Int -> Int -> [Int]
sigmaVector len x = [ if i == x then 1 else 0 | i <- [ 0 .. len - 1 ] ]

choosePair :: Int -> (a,a) -> a
choosePair 0 = fst
choosePair 1 = snd

whenM :: Monad m => m Bool -> m () -> m ()
whenM cond m = do
    ok <- cond
    if ok then m else return ()

interleave :: [a] -> [a] -> [a]
interleave xs ys = concat (zipWith (\x y -> [x,y]) xs ys)
