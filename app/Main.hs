{-# LANGUAGE ExistentialQuantification #-}

module Main where

import Circuit
import Circuit.Conversion
import Circuit.Optimizer
import Circuit.Utils
import qualified Circuit.Format.Acirc    as Acirc
import qualified Circuit.Format.Acirc2   as Acirc2
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

data Mode = ReadCircuit FilePath
          | Garble FilePath
          | CompileAcirc String
          | CompileAcirc2 String
          deriving (Show)

data Opts = Opts { mode                :: Mode
                 , verbose             :: Bool
                 , show_info           :: Bool
                 , ntests              :: Maybe Int
                 , target              :: Maybe String
                 , optimization_level  :: Int
                 , coerce              :: Maybe String
                 , run_tests           :: Bool
                 } deriving (Show)

parseArgs :: IO Opts
parseArgs = execParser $ info (parser <**> helper)
    (fullDesc <> progDesc "cxs is a tool to compile, convert, and optimize circuits")
  where
    parser = Opts
            <$> subparser
                (command "read"
                    (info (readParser <**> helper)
                        (progDesc "Read an existing circuit"))
                <> command "compile"
                    (info (compileParser <**> helper)
                        (progDesc "Compile an acirc"))
                <> command "compile2"
                    (info (compile2Parser <**> helper)
                        (progDesc "Compile an acirc2"))
                <> command "garble"
                    (info (garbleParser <**> helper)
                        (progDesc "Compile a garler for an existing boolean circuit")))
            <*> switch (short 'v' <> help "Verbose mode")
            <*> switch (short 'i' <> help "Print circuit info")
            <*> (optional $ option auto (short 'T' <> metavar "N" <> help "Generate N tests"))
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
            <*> (optional $ strOption
                ( short 't'
                <> metavar "TYPE"
                <> help "Coerce circuit to type TYPE [a,a2,b]"))
            <*> switch (short 'e' <> help "Ensure circuit tests pass")

    readParser    = ReadCircuit <$> strArgument (metavar "CIRCUIT" <> help "The source circuit")
    garbleParser  = Garble <$> strArgument (metavar "CIRCUIT" <> help "The source circuit to garble (must be boolean)")
    compileParser  = CompileAcirc  <$> strArgument (metavar "NAME" <> help "The name of the compilation routine")
    compile2Parser = CompileAcirc2 <$> strArgument (metavar "NAME" <> help "The name of the compilation routine")

main :: IO ()
main = chooseMode =<< parseArgs

chooseMode :: Opts -> IO ()
chooseMode opts = do
    when (verbose opts) (print opts)
    case mode opts of
        CompileAcirc name -> do
            runExportedRoutine name $ M.union
                (include [Point.export, GGM.export, Goldreich.export]) $
                M.fromList
                [ ("aes"           , compileAcirc opts AES.make)
                , ("aes1r"         , compileAcirc opts AES.makeAes1r)
                , ("aes10r"        , compileAcirc opts AES.makeAes10r)
                , ("applebaum"     , compileAcirc opts AR.makeApplebaum)
                , ("tribes"        , compileAcirc opts Tribes.make)
                , ("comparison"    , compileAcirc opts Comparison.make)
                ]

        CompileAcirc2 name -> do
            let m = include2 [ Garbler.export
                             , Goldreich.export
                             , Simple.export
                             ]
            runExportedRoutine name m

        ReadCircuit inp -> do
            let ext = takeExtension inp

            when (ext `notElem` [".acirc", ".acirc2", ".nigel", ".netlist"]) $
                error (printf "[main] unknown input extension \"%s\"!" ext)

            case target opts of
                Nothing -> case ext of
                    ".acirc" -> case coerce opts of
                        Just "a"  -> run opts (Acirc.readWithTests inp)
                        Just "a2" -> run opts (over _1 toAcirc2 <$> Acirc.readWithTests inp)
                        Just _    -> error "[main] supported types for acirc: [a,a2]"
                        Nothing   -> run opts (Acirc.readWithTests inp)
                    ".acirc2" -> case coerce opts of
                        Just "a"  -> run opts (over _1 toAcirc <$> Acirc2.readWithTests inp)
                        Just "a2" -> run opts (Acirc2.readWithTests inp)
                        Just _    -> error "[main] supported types for acirc2: [a,a2]"
                        Nothing   -> run opts (Acirc2.readWithTests inp)
                    ".nigel" -> case coerce opts of
                        Just "a"   -> run opts (Nigel.readNigel inp :: IO (Acirc, [TestCase]))
                        Just "a2"  -> run opts (Nigel.readNigel inp :: IO (Acirc2, [TestCase]))
                        Just "b"   -> run opts (Nigel.readNigel inp :: IO (Acirc2, [TestCase]))
                        Nothing    -> run opts (Nigel.readNigel inp :: IO (Circ, [TestCase]))
                        Just other -> error "[main] supported coersion types for nigel: [a, a2, b]"
                    ".netlist" -> case coerce opts of
                        Just "a"   -> run opts (Netlist.readNetlist inp :: IO (Acirc, [TestCase]))
                        Just "a2"  -> run opts (Netlist.readNetlist inp :: IO (Acirc2, [TestCase]))
                        Just "b"   -> run opts (Netlist.readNetlist inp :: IO (Circ, [TestCase]))
                        Nothing    -> run opts (Netlist.readNetlist inp :: IO (Circ, [TestCase]))
                        Just other -> error "[main] supported coersion types for netlist: [a, a2, b]"

                Just outputFile -> case takeExtension outputFile of
                    ".acirc" -> case ext of
                        ".acirc"   -> run opts (Acirc.readWithTests inp)
                        ".nigel"   -> run opts (Nigel.readNigel inp :: IO (Acirc, [TestCase]))
                        ".netlist" -> run opts (Netlist.readNetlist inp :: IO (Acirc, [TestCase]))
                    ".nigel" -> case ext of
                        ".nigel"   -> run opts (Nigel.readNigel inp :: IO (Circ, [TestCase]))
                        ".netlist" -> run opts (Netlist.readNetlist inp :: IO (Circ, [TestCase]))
                        other      -> error "[main] supported input formats for nigel output: [nigel, netlist]"
                    ".netlist" -> case ext of
                        ".nigel"   -> run opts (Nigel.readNigel inp :: IO (Circ, [TestCase]))
                        ".netlist" -> run opts (Netlist.readNetlist inp :: IO (Circ, [TestCase]))
                        other      -> error "[main] supported input formats for netlist output: [nigel, netlist]"

        Garble inp -> do
            let ext = takeExtension inp
            when (ext `notElem` [".nigel", ".netlist"]) $
                error (printf "[main] \"%s\" is not a known boolean circuit type!" ext)
            c <- case ext of
                ".nigel"   -> Nigel.read inp
                ".netlist" -> Netlist.read inp
            g <- Garbler.garbler c
            circuitMain opts (Just (printf "garbled_%s.acirc" (takeBaseName inp))) g []

  where
    include  = M.unions . map (fmap (compileAcirc opts) . M.fromList)
    include2 = M.unions . map (fmap (compileAcirc2 opts) . M.fromList)

    compileAcirc :: Opts -> [(String, IO Acirc)] -> IO ()
    compileAcirc opts tups = forM_ tups $ \(fname, comp) -> do
        c <- comp
        circuitMain opts (Just fname) c []

    compileAcirc2 :: Opts -> [(String, IO Acirc2)] -> IO ()
    compileAcirc2 opts tups = forM_ tups $ \(fname, comp) -> do
        c <- comp
        circuitMain opts (Just fname) c []

    run opts comp = uncurry (circuitMain opts (target opts)) =<< comp

    runExportedRoutine name m = do
        case M.lookup name m of
            Just c  -> c
            Nothing -> do
                printf "[main] unknown circuit generation mode \"%s\"!\nknown modes:\n" name
                mapM_ (printf "\t%s\n") (M.keys m)
                exitFailure

circuitMain :: (Graphviz.Graphviz g, Optimize g, Gate g, ToAcirc g, ToCirc g, ToAcirc2 g)
            => Opts -> Maybe FilePath -> Circuit g -> [TestCase] -> IO ()
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

    when (run_tests opts) $ void (ensure True c ts)

    case outputName of
        Nothing -> return ()
        Just fn -> do
            let t = case takeExtension fn of
                    ".acirc" -> Acirc.showWithTests (toAcirc c) ts
                    ".acirc2" -> Acirc2.showWithTests (toAcirc2 c) ts
                    ".nigel" -> Nigel.showCirc (toCirc c)
                    ".dot"   -> Graphviz.showGraphviz c
                    other    -> error (printf "[main] unknown output format \"%s\"" other)
            printf "writing %s\n" fn
            T.writeFile fn t


evalTests :: Gate g => Opts -> Circuit g -> [TestCase] -> IO ()
evalTests opts c ts = do
    pr "evaluating plaintext circuit tests"
    ok <- ensure (verbose opts) c ts
    if ok then pr "ok" else pr "failed"
