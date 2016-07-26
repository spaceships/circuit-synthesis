{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE Strict #-}

import Circuit
import Circuit.Builder
import qualified Circuit.Format.Acirc as Acirc
import Util

import Control.Monad
import Data.List.Split
import Debug.Trace
import qualified Data.Vector as V

tribes :: Int -> [Ref] -> Ref -> Builder Ref
tribes k y z = circXor z =<< circOrs =<< mapM circProd (chunksOf k y)

fa :: Int -> Int -> IO Circuit
fa n k = do
    keyBits <- randKeyIO ((n+1)^2)
    return $ buildCircuit $ do
        a <- chunksOf (n+1) <$> secrets keyBits
        x <- inputs (n+1)
        w <- matrixTimesVect a x
        z <- tribes k (init w) (last w)
        output z

fa_128 :: IO Circuit
fa_128 = fa 128 4
