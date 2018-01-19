{-# LANGUAGE ExistentialQuantification #-}

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

import qualified Examples.AES             as AES
import qualified Examples.ApplebaumRaykov as AR
import qualified Examples.Comparison      as Comparison
import qualified Examples.Garbler         as Garbler
import qualified Examples.Goldreich       as Goldreich
import qualified Examples.GGM             as GGM
import qualified Examples.Point           as Point
import qualified Examples.Tribes          as Tribes
import qualified Examples.Simple          as Simple

import Control.Monad
import Lens.Micro.Platform
import System.Exit
import System.FilePath.Posix (takeBaseName, takeExtension)
import Text.Printf
import qualified Data.Map as M
import qualified Data.Text.IO as T

import Options.Applicative
import Data.Semigroup ((<>))

data Mode = ReadCircuit FilePath GlobalOpts
          | Garble FilePath GlobalOpts
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
                        (progDesc "Compile a circuit from the DSL examples"))
                <> command "garble"
                    (info (garbleParser <**> helper)
                        (progDesc "Compile a garler for an existing boolean circuit")))

    readParser    = ReadCircuit
                    <$> strArgument (metavar "CIRCUIT" <> help "The source circuit")
                    <*> globalOptsParser

    garbleParser  = Garble
                    <$> strArgument (metavar "CIRCUIT" <> help "The source circuit to garble (must be boolean)")
                    <*> globalOptsParser

    compileParser = Compile
                    <$> strArgument (metavar "TYPE" <> help "The type of the circuit")
                    <*> strArgument (metavar "NAME" <> help "The name of the compilation routine")
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

main :: IO ()
main = chooseMode =<< parseArgs

chooseMode :: Mode -> IO ()
chooseMode mode = do
    case mode of
        Compile "acirc" name opts -> do
            runExportedRoutine "acirc" name $ M.union
                (include opts [Point.export, GGM.export, Goldreich.export, AES.export]) $
                M.fromList
                [ ("applebaum"     , compileAcirc opts AR.makeApplebaum)
                , ("tribes"        , compileAcirc opts Tribes.make)
                , ("comparison"    , compileAcirc opts Comparison.make)
                ]

        Compile "acirc2" name opts -> do
            let m = include2 opts [ Garbler.export , Goldreich.export , Simple.export ]
            runExportedRoutine "acirc2" name m

        Compile ty _ _ -> error (printf "[main] unsupported type: \"%s\"" ty)

        ReadCircuit inp opts -> do
            let ext = takeExtension inp

            case target opts of
                Nothing -> case ext of
                    ".acirc"   -> run opts (Acirc.readWithTests inp)
                    ".acirc2"  -> run opts (Acirc2.readWithTests inp)
                    ".nigel"   -> run opts (Nigel.readNigel inp :: IO (Circ, [TestCase]))
                    ".netlist" -> run opts (Netlist.readNetlist inp :: IO (Circ, [TestCase]))
                    ".bench"   -> run opts (Bench.readBench inp :: IO (Circ, [TestCase]))
                    other -> error (printf "[main] unknown input extension \"%s\"!" other)

                Just outputFile -> case takeExtension outputFile of
                    ".acirc" -> case ext of
                        ".acirc"   -> run opts (Acirc.readWithTests inp)
                        ".nigel"   -> run opts (Nigel.readNigel inp :: IO (Acirc, [TestCase]))
                        ".netlist" -> run opts (Netlist.readNetlist inp :: IO (Acirc, [TestCase]))
                        other      -> error "[main] supported input formats for acirc output: [acirc, nigel, netlist]"
                    ".acirc2" -> case ext of
                        ".acirc2"  -> run opts (Acirc2.readWithTests inp)
                        ".nigel"   -> run opts (Nigel.readNigel inp :: IO (Acirc2, [TestCase]))
                        ".netlist" -> run opts (Netlist.readNetlist inp :: IO (Acirc2, [TestCase]))
                        other      -> error "[main] supported input formats for acirc2 output: [acirc2, nigel, netlist]"
                    ".nigel" -> case ext of
                        ".nigel"   -> run opts (Nigel.readNigel inp :: IO (Circ, [TestCase]))
                        ".netlist" -> run opts (Netlist.readNetlist inp :: IO (Circ, [TestCase]))
                        other      -> error "[main] supported input formats for nigel output: [nigel, netlist]"
                    ".netlist" -> case ext of
                        ".nigel"   -> run opts (Nigel.readNigel inp :: IO (Circ, [TestCase]))
                        ".netlist" -> run opts (Netlist.readNetlist inp :: IO (Circ, [TestCase]))
                        other      -> error "[main] supported input formats for netlist output: [nigel, netlist]"
                    other -> error (printf "[main] unknown output extension \"%s\"!" other)

        Garble inp opts -> do
            let ext = takeExtension inp
            when (ext `notElem` [".nigel", ".netlist"]) $
                error (printf "[main] \"%s\" is not a known boolean circuit type!" ext)
            c <- case ext of
                ".nigel"   -> Nigel.read inp
                ".netlist" -> Netlist.read inp
            g <- Garbler.naiveGarbler 1 c
            circuitMain opts (Just (printf "garbled_%s.acirc" (takeBaseName inp))) g []

  where
    include  opts = M.unions . map (fmap (compileAcirc opts) . M.fromList)
    include2 opts = M.unions . map (fmap (compileAcirc2 opts) . M.fromList)

    compileAcirc :: GlobalOpts -> [IO (String, Acirc)] -> IO ()
    compileAcirc opts actions = forM_ actions $ \m -> do
        (fname, c) <- m
        circuitMain opts (Just fname) c []

    compileAcirc2 :: GlobalOpts -> [IO (String, Acirc2)] -> IO ()
    compileAcirc2 opts actions = forM_ actions $ \m -> do
        (fname, c) <- m
        circuitMain opts (Just fname) c []

    run opts comp = uncurry (circuitMain opts (target opts)) =<< comp

    runExportedRoutine ty name m = do
        case M.lookup name m of
            Just c  -> c
            Nothing -> do
                printf "[main] unknown circuit generation mode \"%s\"!\n" name
                printf "known modes for %s:\n" ty
                mapM_ (printf "\t%s\n") (M.keys m)
                exitFailure

circuitMain :: (Graphviz.Graphviz g, Optimize g, Gate g, ToAcirc g, ToCirc g, ToAcirc2 g)
            => GlobalOpts -> Maybe FilePath -> Circuit g -> [TestCase] -> IO ()
circuitMain opts outputName c ts = do
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

    ts <- case ntests opts of
        Just n  -> replicateM n (genTest c)
        Nothing -> if not (null ts)
                      then return ts
                      else replicateM 10 (genTest c)

    when (run_tests opts) $ evalTests opts c ts

    case outputName of
        Nothing -> return ()
        Just fn -> do
            let t = case takeExtension fn of
                    ".acirc" -> Acirc.showWithTests (toAcirc c) ts
                    ".acirc2" -> Acirc2.showWithTests (toAcirc2 c) ts
                    ".nigel" -> Nigel.showCirc (toCirc c)
                    ".dot"   -> Graphviz.showGraphviz c
                    other    -> error (printf "[main] unknown output format \"%s\"" other)
            when (verbose opts) $ printf "writing %s\n" fn
            T.writeFile fn t


evalTests :: Gate g => GlobalOpts -> Circuit g -> [TestCase] -> IO ()
evalTests opts c ts = do
    when (verbose opts) $ pr "evaluating plaintext circuit tests"
    ok <- ensure (verbose opts) c ts
    if ok then when (verbose opts) (pr "ok") else pr "failed"
