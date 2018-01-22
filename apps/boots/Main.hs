module Main where

import Circuit
import Circuit.Conversion
import Circuit.Optimizer
import Circuit.Utils
import qualified Circuit.Format.Acirc    as Acirc
import qualified Circuit.Format.Acirc2   as Acirc2
import qualified Circuit.Format.Bench    as Bench
import qualified Circuit.Format.Nigel    as Nigel
import qualified Circuit.Format.Netlist  as Netlist
import qualified Circuit.Format.Graphviz as Graphviz

import Options.Applicative

main = putStrLn "hello world"
