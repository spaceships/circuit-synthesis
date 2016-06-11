module Main where

import Circuit
import Util
import Rand
import qualified Circuit.AcircParser as Acirc

import Control.Monad
import Options
import Text.Printf
import System.Exit

data MainOptions = MainOptions { opt_info     :: Bool
                               , opt_verbose  :: Bool
                               , opt_eval     :: Bool
                               , opt_gentests :: Maybe Int
                               }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> defineOption optionType_bool
            (\o -> o { optionLongFlags   = ["info"]
                     , optionShortFlags  = ['i']
                     , optionDescription = "Show circuit info."
                     })
        <*> defineOption optionType_bool
            (\o -> o { optionShortFlags  = ['v']
                     , optionLongFlags   = ["verbose"]
                     , optionDescription = "Be verbose."
                     })
        <*> defineOption optionType_bool
            (\o -> o { optionShortFlags  = ['e']
                     , optionLongFlags   = ["eval"]
                     , optionDescription = "Evaluate the circuit on tests (random if none exist)."
                     })
        <*> defineOption (optionType_maybe optionType_int)
            (\o -> o { optionLongFlags   = ["gen-tests"]
                     , optionDescription = "Generate tests."
                     })

main :: IO ()
main = runCommand $ \opts args -> do
    let [fp] = args
    (c,ts) <- Acirc.parseCirc <$> readFile fp
    when (opt_info opts) $ printCircInfo c
    ts' <- case opt_gentests opts of
        Nothing -> return ts
        Just i  -> replicateM i (genTest c)
    when (opt_eval opts) $ evalPlaintextCircuit opts c ts'
    exitSuccess

printCircInfo :: Circuit -> IO ()
printCircInfo c = printf "circuit info: depth=%d n=%d m=%d xdegs=%s ydeg=%s total degree=%d\n"
                         (depth c) (ninputs c) (nconsts c)
                         (show (xdegs c)) (show (ydeg c))
                         (sum (ydeg c : (xdegs c)))

evalPlaintextCircuit :: MainOptions -> Circuit -> [TestCase] -> IO ()
evalPlaintextCircuit opts c ts = do
    pr "evaluating plaintext circuit tests"
    ok <- ensure (opt_verbose opts) plainEvalIO c ts
    if ok then pr "ok" else pr "failed"

dirName :: FilePath -> Int -> FilePath
dirName fp λ = fp ++ "." ++ show λ

genTest :: Circuit -> IO TestCase
genTest c = do
    inp <- num2Bits (ninputs c) <$> randIO (randInteger (ninputs c))
    return (reverse inp, plainEval c inp)

getKappa :: Circuit -> Int
getKappa c = δ + 2*n + n*(2*n-1)
  where
    n = ninputs c
    δ = ydeg c + sum (xdegs c)

