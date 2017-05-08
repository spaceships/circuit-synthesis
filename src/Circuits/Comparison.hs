{-# LANGUAGE TupleSections #-}

module Circuits.Comparison where

import Circuit
import Circuit.Builder
import Control.Monad

make :: IO [(Maybe String, Circuit)]
make = return [ (Just "comparison.dsl.acirc", comparison 27 8) ]

-- 10^12 input size
-- racheled as 22 vectors of length 4
-- second input is expected to come in special form
comparison :: Int -> Int -> Circuit
comparison nsyms symlen = buildCircuit $ do
    xs <- replicateM nsyms (inputs symlen)
    ys <- replicateM nsyms (inputs symlen) -- assumed to come in the negated sigma vector form
    zs <- zipWithM squash xs ys
    z  <- circSum zs -- since we zero check at the end, we dont care if z > 1. We dont have to maintain the boolean emulation here.
    output z
  where
    squash as bs = circProd =<< zipWithM circMul as bs
