module Main where

import Circuit
import Circuit.Conversion
import Circuit.Optimizer
import Circuit.Utils
import qualified Circuit.Format.Acirc    as Acirc
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

import Control.Monad
import Lens.Micro.Platform
import System.Exit
import System.FilePath.Posix (takeBaseName, takeExtension)
import Text.Printf
import qualified Data.Map as M
import qualified Data.Text.IO as T

import Options.Applicative
import Data.Semigroup ((<>))

data Mode = Compile String
          | ReadCircuit FilePath
          | Garble FilePath
          deriving (Show)

data Opts = Opts { mode               :: Mode
                 , verbose            :: Bool
                 , show_info          :: Bool
                 , target             :: Maybe String
                 , optimization_level :: Int
                 , coerce             :: Maybe String
                 } deriving (Show)

parseArgs :: IO Opts
parseArgs = execParser $ info (parser <**> helper)
    (fullDesc <> progDesc "cxs is a tool to compile, convert, and optimize circuits")
  where
    parser = Opts
            <$> subparser (command "read"    (info (readParser    <**> helper) (progDesc "read an existing circuit")) <>
                           command "compile" (info (compileParser <**> helper) (progDesc "compile a DSL circuit")) <>
                           command "garble"  (info (garbleParser  <**> helper) (progDesc "compile a garler for an existing boolean circuit")))
            <*> switch
                ( short 'v'
                <> help "Verbose mode")
            <*> switch
                ( short 'i'
                <> help "Print circuit info")
            <*> (optional $ strOption
                ( short 'o'
                <> metavar "FILE"
                <> help "Write output to file FILE in a known circuit format"))
            <*> option auto
                ( short 'O'
                <> showDefault
                <> value 0
                <> metavar "INT"
                <> help "Optimization level")
            <*> (optional $ strOption
                ( short 't'
                <> metavar "TYPE"
                <> help "Coerce circuit to type TYPE [a,a2,b]"))

    readParser    = ReadCircuit <$> strArgument (metavar "CIRCUIT" <> help "The source circuit")
    compileParser = Compile <$> (switch (short 'c' <> help "Compile a DSL circuit from the examples")
                             *> strArgument (metavar "NAME" <> help "The name of the compilation routine"))
    garbleParser  = Garble <$> strArgument (metavar "CIRCUIT" <> help "The source circuit to garble (must be boolean)")

main :: IO ()
main = chooseMode =<< parseArgs

chooseMode :: Opts -> IO ()
chooseMode opts = do
    when (verbose opts) (print opts)
    case mode opts of
        Compile name -> do
            let m = M.union (fmap (compile opts) $ M.fromList Garbler.export) $ M.fromList
                    [ ("goldreich"     , compile opts Goldreich.make)
                    , ("aes"           , compile opts AES.make)
                    , ("aes1r"         , compile opts AES.makeAes1r)
                    , ("aes10r"        , compile opts AES.makeAes10r)
                    , ("ggm"           , compile opts GGM.makeGGM)
                    , ("ggmSigma"      , compile opts GGM.makeGGMSigma)
                    , ("ggmNoPrg"      , compile opts GGM.makeGGMNoPrg)
                    , ("ggmNoPrgSigma" , compile opts GGM.makeGGMNoPrg)
                    , ("ggmSigma256"   , compile opts GGM.makeGGMSigma256)
                    , ("ggmSigma1024"  , compile opts GGM.makeGGMSigma1024)
                    , ("applebaum"     , compile opts AR.makeApplebaum)
                    , ("tribes"        , compile opts Tribes.make)
                    , ("point"         , compile opts Point.make)
                    , ("comparison"    , compile opts Comparison.make)
                    ]
            case M.lookup name m of
                Just c  -> c
                Nothing -> do
                    printf "[main] unknown circuit generation mode \"%s\"!\nknown modes:\n" name
                    mapM_ (printf "\t%s\n") (M.keys m)
                    exitFailure
        ReadCircuit inp -> do
            let ext = takeExtension inp

            when (ext `notElem` [".acirc", ".nigel", ".netlist"]) $
                error (printf "[main] unknown input extension \"%s\"!" ext)

            case target opts of
                Nothing -> case ext of
                    ".acirc" -> case coerce opts of
                        Just "a"  -> run opts (Acirc.readWithTests inp)
                        Just "a2" -> run opts (over _1 toAcirc2 <$> Acirc.readWithTests inp)
                        Just _    -> error "[main] supported types for acirc: [a,a2]"
                        Nothing   -> run opts (Acirc.readWithTests inp)
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
    compile opts tups = forM_ tups $ \(fname, comp) -> do
        c <- comp
        circuitMain opts (Just fname) c []

    run opts comp = uncurry (circuitMain opts (target opts)) =<< comp

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


evalTests :: Gate g => Opts -> Circuit g -> [TestCase] -> IO ()
evalTests opts c ts = do
    pr "evaluating plaintext circuit tests"
    ok <- ensure (verbose opts) c ts
    if ok then pr "ok" else pr "failed"
