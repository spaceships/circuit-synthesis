{-# LANGUAGE TupleSections #-}

module Circuits.Comparison where

import Circuit
import Circuit.Builder
import Control.Monad

make :: IO [(Maybe String, Circuit)]
make = return [ (Just "comparison.dsl.acirc", comparison 27 8) ]

-- 10^12 input size
-- racheled as 22 vectors of length 4
comparison :: Int -> Int -> Circuit
comparison nsyms symlen = buildCircuit $ do
    xs <- replicateM nsyms (inputs symlen)
    ys <- replicateM nsyms (inputs symlen) -- assumed to come in the negated sigma vector form
    zs <- zipWithM squash xs ys
    z  <- circOrs zs
    output z
  where
    squash as bs = circProd =<< zipWithM circMul as bs
