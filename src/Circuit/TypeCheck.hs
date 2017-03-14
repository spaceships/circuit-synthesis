module Circuit.TypeCheck where

import Circuit
import Types

import Data.Map.Strict ((!))

import Debug.Trace

typeOfOp :: Op -> Circuit -> MType
typeOfOp (OpInput _ t) _ = t
typeOfOp op            c = trace (show op) $ mgtM refTs
  where
    refs  = opArgs op
    refTs = map (snd . (circ_refmap c !)) refs
