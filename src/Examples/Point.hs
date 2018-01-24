{-# LANGUAGE TupleSections #-}

module Examples.Point where

import Circuit hiding (ninputs, symlen)
import Circuit.Builder
import Circuit.Utils

import Data.List.Split (chunksOf)
import Control.Monad
import Control.Monad.Trans (lift)

export = [("point", [ ("point",) <$> point 25 10
                    ] )]

point :: Int -> Int -> IO Acirc
point ninputs symlen = buildCircuitT $ do
    let q = (fromIntegral ninputs :: Integer) ^ (fromIntegral symlen :: Integer)
    thePoint <- lift $ randIntegerModIO q
    let nbits = numBits q
        bs    = num2Bits nbits thePoint
        ixs   = map bits2Num $ chunksOf (nbits `div` ninputs) bs
        sels  = map (toSel symlen) ixs
    xs <- replicateM ninputs (symbol symlen)
    ys <- mapM secrets sels
    -- !(!(x1=y1) + !(x2=y2) + ... + !(xn=yn))
    zs <- mapM (circNot <=< circSum <=< uncurry (zipWithM circMul)) (zip xs ys)
    output =<< circNot =<< circSum zs
  where
    toSel n x = [ if i == x then 1 else 0 | i <- [0..n-1] ]
