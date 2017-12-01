{-# LANGUAGE TupleSections #-}

module Examples.Comparison where

import Circuit
import Circuit.Builder
import Control.Monad

make :: IO [(Maybe String, Circuit)]
make = return [ (Just "comparison.dsl.acirc", comparison 9 22) ]

-- returns less-than-or-equals
-- first input comes in unary form for instance 0 -> [1,0,0,0] or 3 -> [0,0,0,1]
-- second input is expected to come in special negated unary form: 0 -> [1,0,0,0], 3 -> [1,1,1,1]
comparison :: Int -> Int -> Circuit
comparison nsyms symlen = buildCircuit $ do
    setSymlen symlen
    xs <- replicateM nsyms (inputs symlen)
    ys <- replicateM nsyms (inputs symlen) -- assumed to come in the negated sigma vector form
    z  <- circSum =<< zipWithM squash xs ys
    output z
  where
    squash as bs = circSum =<< zipWithM circMul as bs
