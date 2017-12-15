{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TupleSections #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE Strict #-}
#endif

module Examples.Tribes where

import Circuit
import Circuit.Builder
import Circuit.Utils

import Control.Monad.Trans (lift)
import Data.List.Split

make :: IO [(Maybe String, Acirc)]
make = sequence
    [ (Just "fa_8.dsl.acirc"   ,) <$> fa 8 4
    , (Just "fa_16.dsl.acirc"  ,) <$> fa 16 4
    , (Just "fa_32.dsl.acirc"  ,) <$> fa 32 4
    , (Just "fa_64.dsl.acirc"  ,) <$> fa 64 4
    , (Just "fa_128.dsl.acirc" ,) <$> fa 128 4
    ]

tribes :: Monad m => Int -> [Ref] -> Ref -> BuilderT ArithGate m Ref
tribes k y z = circXor z =<< circOrs =<< mapM circProd (chunksOf k y)

fa :: Int -> Int -> IO Acirc
fa n k = buildCircuitT $ do
    keyBits <- lift $ randKeyIO ((n+1)^(2 :: Int))
    a <- chunksOf (n+1) <$> secrets keyBits
    x <- inputs (n+1)
    w <- matrixTimesVect a x
    z <- tribes k (init w) (last w)
    output z

fa_8 :: IO Acirc
fa_8 = fa 8 4

fa_16 :: IO Acirc
fa_16 = fa 16 4

fa_32 :: IO Acirc
fa_32 = fa 32 4

fa_64 :: IO Acirc
fa_64 = fa 64 4

fa_128 :: IO Acirc
fa_128 = fa 128 4
