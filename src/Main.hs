module Main where

import Circuit
import Util
import Rand
import Circuit.Builder
import Circuit.Optimizer (flatten, flattenRec, foldConsts)
import qualified Circuit.Format.Acirc   as Acirc
import qualified Circuit.Graphviz       as Graphviz

import Circuits.Aes as Aes
import Circuits.Goldreich as Goldreich
import Circuits.Tribes as Tribes

import Control.Monad
import Options
import Text.Printf
import System.Exit

data MainOptions = MainOptions { opt_info       :: Bool
                               , opt_latex_info :: Bool
                               , opt_verbose    :: Bool
                               , opt_test       :: Bool
                               , opt_gentests   :: Maybe Int
                               , opt_gencirc    :: Maybe String
                               , opt_add_acirc_tests    :: Bool
                               , opt_randomize_secrets  :: Bool
                               , opt_write_to_file      :: Maybe String
                               , opt_optimize           :: Maybe Int
                               , opt_graphviz           :: Bool
                               }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> defineOption optionType_bool
            (\o -> o { optionLongFlags   = ["info"]
                     , optionShortFlags  = "i"
                     , optionDescription = "Show circuit info."
                     })

        <*> defineOption optionType_bool
            (\o -> o { optionLongFlags   = ["latex"]
                     , optionShortFlags  = "l"
                     , optionDescription = "Show circuit info as a latex table."
                     })

        <*> defineOption optionType_bool
            (\o -> o { optionShortFlags  = "v"
                     , optionLongFlags   = ["verbose"]
                     , optionDescription = "Be verbose."
                     })

        <*> defineOption optionType_bool
            (\o -> o { optionShortFlags  = "t"
                     , optionLongFlags   = ["test"]
                     , optionDescription = "Run the circuit on tests (random if none exist)."
                     })

        <*> defineOption (optionType_maybe optionType_int)
            (\o -> o { optionLongFlags   = ["gen-tests"]
                     , optionDescription = "Generate tests."
                     })

        <*> defineOption (optionType_maybe optionType_string)
            (\o -> o { optionLongFlags   = ["gen-circuit"]
                     , optionShortFlags  = "C"
                     , optionDescription = "Generate a circuit"
                     })

        <*> defineOption optionType_bool
            (\o -> o { optionShortFlags  = "A"
                     , optionLongFlags   = ["add-tests"]
                     , optionDescription = "Add tests to the acirc file"
                     })

        <*> defineOption optionType_bool
            (\o -> o { optionShortFlags  = "r"
                     , optionLongFlags   = ["randomize-secrets"]
                     , optionDescription = "Randomize the key"
                     })

        <*> defineOption (optionType_maybe optionType_string)
            (\o -> o { optionShortFlags  = "o"
                     , optionLongFlags   = ["output"]
                     , optionDescription = "Write the circuit to file FILE"
                     })

        <*> defineOption (optionType_maybe optionType_int)
            (\o -> o { optionShortFlags  = "O"
                     , optionLongFlags   = ["optimize"]
                     , optionDescription = "Post-compilation optimization level: 1=FoldConsts, 2=FoldConsts&FlattenRec, 3=FoldConsts&Flatten"
                     })

        <*> defineOption optionType_bool
            (\o -> o { optionShortFlags  = "d"
                     , optionLongFlags   = ["dot"]
                     , optionDescription = "Output the circuit in Graphviz dot format"
                     })


main :: IO ()
main = runCommand $ \opts args -> do
    case opt_gencirc opts of
        Just "aes"           -> mapM_ (circuitMain opts []) =<< Aes.make
        Just "goldreich"     -> mapM_ (circuitMain opts []) =<< Goldreich.makePRG
        Just "ggm"           -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGM
        Just "ggmSigma"      -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGMSigma
        Just "ggmNoPrg"      -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGMNoPrg
        Just "ggmNoPrgSigma" -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGMNoPrg
        Just "applebaum"     -> mapM_ (circuitMain opts []) =<< Goldreich.makeApplebaum
        Just "tribes"        -> mapM_ (circuitMain opts []) =<< Tribes.make
        Just _ -> do
            putStrLn "[error] known circuit generation modes: aes, goldreich, ggm, ggmSigma, ggmNoPrg, ggmNoPrgSigma, applebaum, tribes, gf28Mult"
            exitFailure

        Nothing -> do
            when (null args) $ do
                putStrLn "[error] input circuit or compilation mode required"
                exitFailure

            let inputFile = head args

            when (opt_add_acirc_tests opts) $ do
                Acirc.addTestsToFile inputFile
                printf "added tests to file %s. quitting..." inputFile

            (c,ts) <- Acirc.readAcirc inputFile

            circuitMain opts ts (opt_write_to_file opts, c)

circuitMain :: MainOptions -> [TestCase] -> (Maybe String, Circuit) -> IO ()
circuitMain opts ts (outputName, c) = do
    let old_symlen = circ_symlen c

    c' <- case opt_optimize opts of
        Nothing -> return c
        Just 1  -> return (foldConsts c)
        Just 2  -> flattenRec (foldConsts c)
        Just 3  -> flatten (foldConsts c)
        Just x  -> do
            printf "[error] unknown optimization level %d\n" x
            exitFailure

    let c = c' { circ_symlen = old_symlen }

    c <- if opt_randomize_secrets opts
            then randomizeSecrets c
            else return c

    when (opt_info opts) $ do
        printCircInfo c

    when (opt_latex_info opts) $ do
        printCircInfoLatex c

    ts <- case opt_gentests opts of
        Nothing -> return ts
        Just i  -> replicateM i (genTest c)

    when (opt_test opts) $ do
        evalTests opts c ts

    s <- if opt_graphviz opts
            then return $ Graphviz.showCircuit c
            else Acirc.showCircWithTests 10 c

    case outputName of
        Just fn -> do
            printf "writing %s\n" fn
            writeFile fn s
        Nothing -> return ()

evalTests :: MainOptions -> Circuit -> [TestCase] -> IO ()
evalTests opts c ts = do
    pr "evaluating plaintext circuit tests"
    ok <- ensure (opt_verbose opts) c ts
    if ok then pr "ok" else pr "failed"

dirName :: FilePath -> Int -> FilePath
dirName fp λ = fp ++ "." ++ show λ

getKappa :: Circuit -> Integer
getKappa c = δ + 2*n + n*(2*n-1)
  where
    n = fromIntegral $ ninputs c
    δ = sum (degs c)
