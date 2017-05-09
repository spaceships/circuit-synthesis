{-# LANGUAGE TupleSections #-}

module Circuits.Comparison where

import Circuit
import Circuit.Builder
import Control.Monad

make :: IO [(Maybe String, Circuit)]
make = return [ (Just "comparison.dsl.acirc", comparison 9 22) ]

-- TODO: this is not comparison- fix it using comparison.cry
-- 10^12 input size
-- racheled as 22 vectors of length 4
-- second input is expected to come in special form
comparison :: Int -> Int -> Circuit
comparison nsyms symlen = buildCircuit $ do
    setSymlen symlen
    xs <- replicateM nsyms (inputs symlen)
    ys <- replicateM nsyms (inputs symlen) -- assumed to come in the negated sigma vector form
    z  <- circSum =<< zipWithM squash xs ys
    output z
  where
    squash as bs = circSum =<< zipWithM circMul as bs
