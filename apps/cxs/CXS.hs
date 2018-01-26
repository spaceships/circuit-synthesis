{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Circuit
import Circuit.Conversion
import Circuit.Optimizer
import Circuit.Utils
import Circuit.Format.Graphviz (Graphviz)
import Circuit.Optimizer (Optimize)
import qualified Circuit.Format.Acirc    as Acirc
import qualified Circuit.Format.Acirc2   as Acirc2
import qualified Circuit.Format.Bench    as Bench
import qualified Circuit.Format.Circ     as Circ
import qualified Circuit.Format.Nigel    as Nigel
import qualified Circuit.Format.Netlist  as Netlist
import qualified Circuit.Format.Graphviz as Graphviz

import qualified Examples.AES             as AES
import qualified Examples.ApplebaumRaykov as AR
import qualified Examples.Comparison      as Comparison
import qualified Examples.Garbler         as Garbler
import qualified Examples.Goldreich       as Goldreich
import qualified Examples.GGM             as GGM
import qualified Examples.Point           as Point
import qualified Examples.Simple          as Simple

import Control.Monad
import Data.List.Split (splitOn)
import Data.Semigroup ((<>))
import Lens.Micro.Platform
import Options.Applicative
import System.Exit
import System.FilePath.Posix (takeBaseName, takeExtension)
import Text.Printf
import qualified Data.Map as M
import qualified Data.Text.IO as T

data Mode = ReadCircuit FilePath GlobalOpts
          | Compile String String GlobalOpts
          deriving (Show)

data GlobalOpts = GlobalOpts
                { verbose             :: Bool
                , show_info           :: Bool
                , ntests              :: Maybe Int
                , target              :: Maybe String
                , optimization_level  :: Int
                , run_tests           :: Bool
                , fix_inputs          :: Maybe String
                , show_truth_table    :: Bool
                } deriving (Show)

parseArgs :: IO Mode
parseArgs = execParser $ info (parser <**> helper)
    (fullDesc <> progDesc "cxs is a tool to compile, convert, and optimize circuits")
  where
    parser = subparser
                (command "read"
                    (info (readParser <**> helper)
                        (progDesc "Read an existing circuit"))
                <> command "compile"
                    (info (compileParser <**> helper)
                        (progDesc "Compile a circuit from the DSL examples")))

    readParser = ReadCircuit
                    <$> strArgument (metavar "CIRCUIT" <> help "The source circuit")
                    <*> globalOptsParser

    compileParser = Compile
                    <$> strArgument (metavar "TYPE" <> help "The type of the circuit [\"acirc\", \"acirc2\", \"circ\"]")
                    <*> strArgument (metavar "NAME" <> help "The name of the compilation routine \
                                                            \ (\"list\" to see available routines)")
                    <*> globalOptsParser

    globalOptsParser = GlobalOpts
                    <$> switch (short 'v' <> help "Verbose mode")
                    <*> switch (short 'i' <> help "Print circuit info")
                    <*> (optional $ option auto (short 'T' <> metavar "N" <> help "Generate N fresh tests"))
                    <*> (optional $ strOption
                        ( short 'o'
                        <> metavar "FILE"
                        <> help "Write output to file FILE in a known circuit format"))
                    <*> option auto
                        ( short 'O'
                        <> showDefault
                        <> value 0
                        <> metavar "N"
                        <> help "Optimization level")
                    <*> switch (short 'e' <> help "Ensure circuit tests pass")
                    <*> (optional $ strOption
                        ( short 'k'
                        <> metavar "STRING"
                        <> help "Fix inputs as secrets. Example: \"0:1 1:0\" fixes the 0th input to \
                               \ 1 and the 1st input to 0. Error thrown if secret occurs in a symbol."))
                    <*> switch (short 'T' <> help "Print truth table")

main :: IO ()
main = chooseMode =<< parseArgs

chooseMode :: Mode -> IO ()
chooseMode mode = do
    case mode of
        Compile "acirc" name opts -> do
            let m = include "acirc" opts [Point.export, AR.export, Comparison.export, GGM.export,
                                 Goldreich.export, AES.export, Simple.export]
            runExportedRoutine "acirc" name m

        Compile "acirc2" name opts -> do
            let m = include "acirc2" opts [ Garbler.export , Goldreich.export, Simple.export ]
            runExportedRoutine "acirc2" name m

        Compile "circ" name opts -> do
            let m = include "circ" opts [ Simple.export :: [(String, [IO (String, Circ)])]]
            runExportedRoutine "circ" name m

        Compile ty _ _ -> do
            printf "[main] unsupported type: \"%s\"!\n" ty
            putStrLn "available types for circuit compilation: \"acirc\", \"acirc2\", \"circ\""
            exitFailure

        ReadCircuit inp opts -> do

            case target opts of
                Nothing -> case takeExtension inp of
                    ".acirc"   -> run opts (Acirc.readWithTests inp :: IO (Acirc, [TestCase]))
                    ".acirc2"  -> run opts (Acirc2.readWithTests inp :: IO (Acirc2, [TestCase]))
                    ".circ"    -> run opts (Circ.readWithTests inp :: IO (Circ, [TestCase]))
                    ".nigel"   -> run opts (Nigel.readNigel inp :: IO (Circ, [TestCase]))
                    ".netlist" -> run opts (Netlist.readNetlist inp :: IO (Circ, [TestCase]))
                    ".bench"   -> run opts (Bench.readBench inp :: IO (Circ, [TestCase]))
                    other -> error (printf "[main] unknown input extension \"%s\"!" other)

                Just outputFile -> case takeExtension outputFile of
                    ".acirc" -> do
                        (c,ts) <- readFormat inp
                        circuitMain opts (target opts) (c :: Acirc) ts

                    ".acirc2" -> do
                        (c,ts) <- readFormat inp
                        circuitMain opts (target opts) (c :: Acirc2) ts

                    -- boolean formats
                    outExt -> do
                        when (outExt `notElem` [".circ", ".nigel"])
                            (error (printf "[main] unsupported output extension \"%s\"!" outExt))
                        (c,ts) <- readFormat inp
                        circuitMain opts (target opts) (c :: Circ) ts

  where
    include ty opts = M.unions . map (fmap (compile ty opts) . M.fromList)

    compile :: (ToAcirc g, ToAcirc2 g, ToCirc g, Graphviz g, Optimize g, Gate g)
            => String -> GlobalOpts -> [IO (String, Circuit g)] -> IO ()
    compile ty opts actions = forM_ actions $ \m -> do
        (fname, c) <- m
        circuitMain opts (Just (fname ++ "." ++ ty)) c []

    run opts comp = uncurry (circuitMain opts (target opts)) =<< comp

    runExportedRoutine ty name m = do
        case M.lookup name m of
            Just c  -> c
            Nothing -> do
                printf "[main] unknown circuit generation mode \"%s\"!\n" name
                printf "known modes for %s:\n" ty
                mapM_ (printf "\t%s\n") (M.keys m)
                exitFailure

circuitMain :: (Graphviz g, Optimize g, Gate g, ToAcirc g, ToCirc g, ToAcirc2 g)
            => GlobalOpts -> Maybe FilePath -> Circuit g -> [TestCase] -> IO ()
circuitMain opts outputName inputC ts = do

    let c = case fix_inputs opts of
                Nothing -> inputC
                Just st -> fixInputBits (parseFixedInputsString st) inputC

    let old_symlen = _circ_symlen c
    c' <- case optimization_level opts of
        0 -> return c
        1 -> optimizeO1 c
        2 -> optimizeO2 c
        3 -> optimizeO3 c
        x -> do
            printf "[error] unknown optimization level %d\n" x
            exitFailure
    let c = c' & circ_symlen .~ old_symlen

    when (show_info opts) $ do
        printCircInfo c
        when (verbose opts) $
            printf "\tvar-degree=%s\n" (unwords (map show (degs c)))

    ts <- case ntests opts of
        Just n  -> replicateM n (genTest c)
        Nothing -> if not (null ts)
                      then return ts
                      else replicateM 10 (genTest c)

    when (run_tests opts) $ evalTests opts c ts

    when (show_truth_table opts) $ printTruthTable c

    case outputName of
        Nothing -> return ()
        Just fn -> do
            let t = case takeExtension fn of
                    ".circ"   -> Circ.showWithTests (toCirc c) ts
                    ".acirc"  -> Acirc.showWithTests (toAcirc c) ts
                    ".acirc2" -> Acirc2.showWithTests (toAcirc2 c) ts
                    ".nigel"  -> Nigel.showCirc (toCirc c)
                    ".dot"    -> Graphviz.showGraphviz c
                    other     -> error (printf "[main] unknown output format \"%s\"" other)
            when (verbose opts) $ printf "writing %s\n" fn
            T.writeFile fn t


evalTests :: Gate g => GlobalOpts -> Circuit g -> [TestCase] -> IO ()
evalTests opts c ts = do
    when (verbose opts) $ pr "evaluating plaintext circuit tests"
    ok <- ensure (verbose opts) c ts
    if ok then when (verbose opts) (pr "ok") else pr "failed"

parseFixedInputsString :: String -> [(InputId, Int)]
parseFixedInputsString s = map (f . splitOn ":") (words s)
  where
    f [sx,sy] = (InputId (read sx), read sy)
    f _ = error (printf "[parseFixedInputsString] couldn't read string \"%s\"!" s)

readFormat :: Gate g => FilePath -> IO (Circuit g, [TestCase])
readFormat inp = case takeExtension inp of
    ".acirc"   -> Acirc.readWithTests inp
    ".acirc2"  -> Acirc2.readWithTests inp
    ".circ"    -> Circ.readWithTests inp
    ".nigel"   -> Nigel.readNigel inp
    ".netlist" -> Netlist.readNetlist inp
    ".bench"   -> Bench.readBench inp
    other      -> error (printf "[readFormat] unsupported input extension: \"%s\"!" other)
