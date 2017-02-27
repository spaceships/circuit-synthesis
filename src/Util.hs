{-# LANGUAGE MagicHash #-}

module Util where

import Data.Bits ((.&.))
import System.IO
import Control.DeepSeq (deepseq)
import Control.Parallel.Strategies
import GHC.Types
import qualified GHC.Integer.GMP.Internals as GMP
import qualified Data.Map as M

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

num2Bits :: Int -> Integer -> [Bool]
num2Bits n x = reverse bs
  where
    bs = [ x .&. 2^i > 0 | i <- [0 .. n-1] ]

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
permutations n possibleValues = sequence (replicate n possibleValues)

-- all possible n-tuples of True, False
booleanPermutations :: Int -> [[Bool]]
booleanPermutations n = permutations n [False, True]

transpose :: [[a]] -> [[a]]
transpose xs | null (head xs) = []
             | otherwise = map head xs : transpose (map tail xs)


numBits :: Int -> Int
numBits n = ceiling (logBase 2 (fromIntegral n) :: Double)
