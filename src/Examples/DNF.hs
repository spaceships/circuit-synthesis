{-# LANGUAGE TupleSections #-}

module Examples.DNF where

import Circuit
import Circuit.Builder

import Data.List (transpose)

export :: Gate g => [(String, [IO (String, Circuit g)])]
export = [ ("3dnf", [ ("3dnf_8",) <$> return (dnf 8)
                    , ("3dnf_16",) <$> return (dnf 16)
                    , ("3dnf_32",) <$> return (dnf 32)
                    , ("3dnf_64",) <$> return (dnf 64)
                    , ("3dnf_128",) <$> return (dnf 128)
                    , ("3dnf_256",) <$> return (dnf 256)
                    , ("3dnf_512",) <$> return (dnf 512)
                    , ("3dnf_1024",) <$> return (dnf 1024)
                    , ("3dnf_2048",) <$> return (dnf 2048)
                    ])]


dnf :: Gate g => Int -> Circuit g
dnf nbits = buildCircuit $ do
    xs <- symbol nbits
    ys <- symbol nbits
    zs <- symbol nbits
    output =<< circOrs =<< mapM circProd (transpose [xs,ys,zs])
