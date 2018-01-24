{-# LANGUAGE TupleSections #-}

module Examples.Comparison where

import Circuit
import Circuit.Builder
import Control.Monad

export :: [(String, [IO (String, Acirc)])]
export = [("comparison", [return ("comparison", comparison 9 22)])]

-- returns less-than-or-equals
-- first input comes in unary form for instance 0 -> [1,0,0,0] or 3 -> [0,0,0,1]
-- second input is expected to come in special negated unary form: 0 -> [1,0,0,0], 3 -> [1,1,1,1]
comparison :: Int -> Int -> Circuit ArithGate
comparison nsyms symlen = buildCircuit $ do
    xs <- replicateM nsyms (symbol symlen)
    ys <- replicateM nsyms (symbol symlen) -- assumed to come in the negated sigma vector form
    z  <- circSum =<< zipWithM squash xs ys
    output z
  where
    squash as bs = circSum =<< zipWithM circMul as bs
