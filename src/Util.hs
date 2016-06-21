{-# LANGUAGE MagicHash #-}

module Util where

import Data.Bits ((.&.))
import System.IO
import Control.DeepSeq (deepseq)
import Control.Parallel.Strategies
import GHC.Prim
import GHC.Types
import qualified GHC.Integer.GMP.Internals as GMP
import qualified Data.Map as M

sizeBase2 :: Integer -> Int
sizeBase2 x = fromIntegral (W# (GMP.sizeInBaseInteger x 2#))

invMod :: Integer -> Integer -> Integer
invMod x q = GMP.recipModInteger x q

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

readBitstring :: String -> [Bool]
readBitstring s = if take 2 s == "0b"
                     then map (== '1') (drop 2 s)
                     else error "[readBitstring] unknown bitstring"

showBitstring :: [Bool] -> String
showBitstring bs = "0b" ++ showBitstring' bs

showBitstring' :: [Bool] -> String
showBitstring' bs = map (\b -> if b then '1' else '0') bs

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

