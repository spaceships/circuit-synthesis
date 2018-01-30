module Examples.Substring where

import Circuit
import Circuit.Builder
import Circuit.Utils

import Control.Monad

export :: Gate g => [(String, [IO (String, Circuit g)])]
export = [("substring", [return ("progress", substring 10 "progress")])]

-- XXX: input length is number of ascii characters!
substring :: Gate g => Int -> String -> Circuit g
substring inputLen s = buildCircuit $ do
    inp <- symbol (inputLen * 8)
    pat <- secrets (map b2i (str2Bits s))
    let len  = length pat
        diff = inputLen * 8 - len
    when (diff < 0) $
        error "[substring] input shorter than pattern!"
    comps <- forM [0..diff] $ \offset -> do
        let xs = take len (drop offset inp)
        circProd =<< zipWithM circEq xs pat
    output =<< circOrs comps
