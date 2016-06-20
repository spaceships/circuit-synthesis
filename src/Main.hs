module Main where

import Circuit
import Util
import Rand
import Circuit.Parser (CircuitParser)
import qualified Circuit.AcircParser    as Acirc
import qualified Circuit.VerilogParser  as Verilog

import Control.Monad
import Options
import Text.Printf
import System.Exit

data MainOptions = MainOptions { opt_info     :: Bool
                               , opt_verbose  :: Bool
                               , opt_test     :: Bool
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
            (\o -> o { optionShortFlags  = ['t']
                     , optionLongFlags   = ["test"]
                     , optionDescription = "Run the circuit on tests (random if none exist)."
                     })
        <*> defineOption (optionType_maybe optionType_int)
            (\o -> o { optionLongFlags   = ["gen-tests"]
                     , optionDescription = "Generate tests."
                     })

main :: IO ()
main = runCommand $ \opts args -> do
    when (length args == 0) $ do
         putStrLn "[error] input circuit required"
         exitFailure
    let inputFile = head args
        parser    = parserFor inputFile :: CircuitParser
    (c,ts) <- parser <$> readFile inputFile
    when (opt_info opts) $ printCircInfo c
    ts' <- case opt_gentests opts of
        Nothing -> return ts
        Just i  -> replicateM i (genTest c)
    when (opt_test opts) $ evalTests opts c ts'
    exitSuccess

evalTests :: MainOptions -> Circuit -> [TestCase] -> IO ()
evalTests opts c ts = do
    pr "evaluating plaintext circuit tests"
    ok <- ensure (opt_verbose opts) plainEvalIO c ts
    if ok then pr "ok" else pr "failed"

dirName :: FilePath -> Int -> FilePath
dirName fp λ = fp ++ "." ++ show λ

getKappa :: Circuit -> Int
getKappa c = δ + 2*n + n*(2*n-1)
  where
    n = ninputs c
    δ = ydeg c + sum (xdegs c)

parserFor :: String -> CircuitParser
parserFor filename = case extension filename of
    "acirc" -> Acirc.parseCirc
    "v"     -> Verilog.parseCirc
    ext     -> error $ printf "[error] unknown circuit type: \"%s\"" ext
  where
    extension = reverse . takeWhile (/= '.') . reverse
