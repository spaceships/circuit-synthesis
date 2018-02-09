{-# LANGUAGE TupleSections #-}

module Examples.Comparison where

import Circuit
import Circuit.Builder
import Circuit.Utils
import Control.Monad

export :: [(String, [IO (String, Acirc)])]
export = [("comparison", [return ("comparison", comparison 20)])]

-- returns less-than-or-equals
-- first input comes in unary form for instance 0 -> [1,0,0,0] or 3 -> [0,0,0,1]
-- second input is expected to come in special negated unary form: 0 -> [1,0,0,0], 3 -> [1,1,1,1]
comparison :: Int -> Circuit ArithGate
comparison maxVal = buildCircuit $ do
    xs <- symbol maxVal
    ys <- symbol maxVal -- assumed to come in the negated sigma vector form
    z  <- circSum =<< zipWithM circMul xs ys
    output z

makeComparisonInput :: Int -> Int -> Int -> ([Int], [Int])
makeComparisonInput max x y = (sigmaVector max x, yvec)
  where
    yvec = replicate (y + 1) 1 ++ replicate (max - y - 1) 0

testComparison :: Int -> Int -> Int -> Int
testComparison max x y = head (plainEval (comparison max) (xs ++ ys))
  where
    (xs,ys) = makeComparisonInput max x y
