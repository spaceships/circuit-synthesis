module Main where


import Circuit
import Util
import Rand
import Circuit.Builder
import Circuit.Parser (CircuitParser)
import Circuit.Graphviz
import qualified Circuit.Format.Acirc   as Acirc
import qualified Circuit.Format.Verilog as Verilog

import Circuits.Aes as Aes
import Circuits.Goldreich as Goldreich
import Circuits.Tribes as Tribes

import Control.Monad
import System.Exit
import System.Environment

import qualified Data.Text.Lazy.IO as LT
import System.FilePath

main :: IO ()
main = do
  fs <- getArgs
  when (null fs) $ do
      putStrLn "[error] input circuit required"
      exitFailure
  let parser f  = parserFor f :: CircuitParser
  cs <- mapM (\f -> fst . parser f <$> readFile f) fs
  let docs  = fmap showCircuitT cs
      outfs = fmap (flip replaceExtension ".dot") fs
      wActs = fmap LT.writeFile outfs
  sequence_ $ zipWith ($) wActs docs
  exitSuccess

parserFor :: String -> CircuitParser
parserFor filename = case takeExtension filename of
    ".acirc" -> Acirc.parseCirc
    ".v"     -> Verilog.parseCirc
    ext     -> error $ "[error] unknown circuit type: " ++ ext
