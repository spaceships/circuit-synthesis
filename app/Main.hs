module Main where

import Circuit
import Circuit.Conversion
import Circuit.Optimizer
import Circuit.Utils
import qualified Circuit.Format.Acirc    as Acirc
import qualified Circuit.Format.Nigel    as Nigel
import qualified Circuit.Format.Netlist  as Netlist
import qualified Circuit.Format.Graphviz as Graphviz

import qualified Examples.Aes             as Aes
import qualified Examples.ApplebaumRaykov as AR
import qualified Examples.Comparison      as Comparison
import qualified Examples.Garbler         as Garbler
import qualified Examples.Goldreich       as Goldreich
import qualified Examples.Point           as Point
import qualified Examples.Tribes          as Tribes

import Control.Monad
import Lens.Micro.Platform
import Options
import System.Exit
import System.FilePath.Posix (takeExtension)
import Text.Printf
import qualified Data.Text.Lazy.IO as T

data MainOptions = MainOptions { opt_info       :: Bool
                               , opt_verbose    :: Bool
                               , opt_test       :: Bool
                               , opt_gentests   :: Maybe Int
                               , opt_gencirc    :: Maybe String
                               , opt_randomize_secrets  :: Bool
                               , opt_write_to_file      :: Maybe String
                               , opt_optimize           :: Maybe Int
                               , opt_circuit_type       :: String
                               }

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> defineOption optionType_bool
            (\o -> o { optionLongFlags   = ["info"]
                     , optionShortFlags  = "i"
                     , optionDescription = "Show circuit info."
                     })

        <*> defineOption optionType_bool
            (\o -> o { optionShortFlags  = "v"
                     , optionLongFlags   = ["verbose"]
                     , optionDescription = "Be verbose."
                     })

        <*> defineOption optionType_bool
            (\o -> o { optionShortFlags  = "T"
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
            (\o -> o { optionShortFlags  = "r"
                     , optionLongFlags   = ["randomize-secrets"]
                     , optionDescription = "Randomize the key"
                     })

        <*> defineOption (optionType_maybe optionType_string)
            (\o -> o { optionShortFlags  = "o"
                     , optionLongFlags   = ["output"]
                     , optionDescription = "Write the circuit to file FILE. Supported filetypes:\
                                           \ *.dot, *.acirc, *.nigel"
                     })

        <*> defineOption (optionType_maybe optionType_int)
            (\o -> o { optionShortFlags  = "O"
                     , optionLongFlags   = ["optimize"]
                     , optionDescription = "Post-compilation optimization level: 1=FoldConsts,\
                                           \ 2=FoldConsts&FlattenRec, 3=FoldConsts&Flatten"
                     })

        <*> defineOption optionType_string
            (\o -> o { optionLongFlags   = ["type"]
                     , optionShortFlags  = "t"
                     , optionDescription = "type of circuit [a,b]"
                     , optionDefault = "a"
                     })

main :: IO ()
main = runCommand $ \opts args -> do
    case opt_gencirc opts of
        Just "aes"           -> mapM_ (circuitMain opts []) =<< Aes.make
        Just "aes1r"         -> mapM_ (circuitMain opts []) =<< Aes.makeAes1r
        Just "aes10r"        -> mapM_ (circuitMain opts []) =<< Aes.makeAes10r
        Just "goldreich"     -> mapM_ (circuitMain opts []) =<< Goldreich.makePRG
        Just "ggm"           -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGM
        Just "ggmSigma"      -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGMSigma
        Just "ggmNoPrg"      -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGMNoPrg
        Just "ggmNoPrgSigma" -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGMNoPrg
        Just "applebaum"     -> mapM_ (circuitMain opts []) =<< AR.makeApplebaum
        Just "tribes"        -> mapM_ (circuitMain opts []) =<< Tribes.make
        Just "point"         -> mapM_ (circuitMain opts []) =<< Point.make
        Just "comparison"    -> mapM_ (circuitMain opts []) =<< Comparison.make
        Just "ggmSigma256"   -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGMSigma256
        Just "ggmSigma1024"  -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGMSigma1024
        Just "size_test"     -> mapM_ (circuitMain opts []) =<< Garbler.makeSizeTest
        Just _ -> do
            putStrLn "[error] known circuit generation modes: aes, aes1r, aes10r, goldreich, ggm,\
                     \ ggmSigma, ggmSigma256, ggmSigma1024, ggmNoPrg, ggmNoPrgSigma, applebaum,\
                     \ tribes, gf28Mult, point, comparison, size_test"
            exitFailure

        Nothing -> do
            when (null args) $ do
                putStrLn "[error] input circuit or compilation mode required"
                exitFailure

            let inputFile = head args

            case opt_circuit_type opts of
                "a" -> do
                    putStrLn "* arithmetic circuit mode"
                    (c,ts) <- case takeExtension inputFile of
                        ".acirc"   -> Acirc.readWithTests inputFile
                        ".nigel"   -> Nigel.readNigel inputFile
                        ".netlist" -> Netlist.readNetlist inputFile
                        s -> error (printf "[main] unkown extension: %s!" s)
                    circuitMain opts ts (opt_write_to_file opts, c :: Acirc)
                "b" -> do
                    putStrLn "* binary circuit mode"
                    (c,ts) <- case takeExtension inputFile of
                        ".acirc"   -> over _1 toCirc <$> Acirc.readWithTests inputFile
                        ".nigel"   -> Nigel.readNigel inputFile
                        ".netlist" -> Netlist.readNetlist inputFile
                        s -> error (printf "[main] unkown extension: %s!" s)
                    circuitMain opts ts (opt_write_to_file opts, c :: Circ)
                other -> error (printf "[main] unknown circuit type %s!" other)

circuitMain :: (Graphviz.Graphviz g, Optimize g, Gate g, ToAcirc g, ToCirc g)
            => MainOptions -> [TestCase] -> (Maybe String, Circuit g) -> IO ()
circuitMain opts ts (outputName, c) = do
    let old_symlen = _circ_symlen c

    c' <- case opt_optimize opts of
        Nothing -> return c
        Just 1  -> optimizeO1 c
        Just 2  -> optimizeO2 c
        Just 3  -> optimizeO3 c
        Just x  -> do
            printf "[error] unknown optimization level %d\n" x
            exitFailure

    let c = c' { _circ_symlen = old_symlen }

    c <- if opt_randomize_secrets opts
            then randomizeSecrets c
            else return c

    when (opt_info opts) $ do
        printCircInfo c

    ts <- case opt_gentests opts of
        Nothing -> return ts
        Just i  -> replicateM i (genTest c)

    when (opt_test opts) $ do
        evalTests opts c ts

    case outputName of
        Nothing -> return ()
        Just fn -> do
            let t = case takeExtension fn of
                    ".acirc" -> Acirc.showWithTests (toAcirc c) ts
                    ".nigel" -> Nigel.showCirc (toCirc c)
                    ".dot"   -> Graphviz.showGraphviz c
                    other    -> error (printf "[main] unknown output format \"%s\"" other)
            printf "writing %s\n" fn
            T.writeFile fn t

evalTests :: Gate g => MainOptions -> Circuit g -> [TestCase] -> IO ()
evalTests opts c ts = do
    pr "evaluating plaintext circuit tests"
    ok <- ensure (opt_verbose opts) c ts
    if ok then pr "ok" else pr "failed"

dirName :: FilePath -> Int -> FilePath
dirName fp λ = fp ++ "." ++ show λ
