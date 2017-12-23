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
import System.Exit
import System.FilePath.Posix (takeExtension)
import Text.Printf
import qualified Data.Text.Lazy.IO as T

import Options.Applicative
import Data.Semigroup ((<>))

data Source = Compile String
            | ReadFile String

data Opts = Opts { source    :: String
                 , compile   :: Bool
                 , verbose   :: Bool
                 , show_info :: Bool
                 , target    :: Maybe String
                 , optimization_level :: Int
                 , coerce :: Maybe String
                 }

parseArgs :: IO Opts
parseArgs = execParser $ info (parser <**> helper)
    (fullDesc <> progDesc "cxs is a tool to compile, convert, and optimize circuits")
  where
    parser = Opts
            <$> strArgument
                ( metavar "SOURCE"
                <> help "The source circuit file or circuit to compile")
            <*> switch
                ( short 'c'
                <> help "Compile a DSL circuit from the examples")
            <*> switch
                ( short 'v'
                <> help "Verbose mode")
            <*> switch
                ( short 'i'
                <> help "Print circuit info")
            <*> (optional $ strOption
                ( short 'o'
                <> metavar "FILE"
                <> help "Write to file FILE"))
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


main :: IO ()
main = do
    opts <- parseArgs

    if compile opts then
        case source opts of
            "aes"           -> mapM_ (circuitMain opts []) =<< Aes.make
            "aes1r"         -> mapM_ (circuitMain opts []) =<< Aes.makeAes1r
            "aes10r"        -> mapM_ (circuitMain opts []) =<< Aes.makeAes10r
            "goldreich"     -> mapM_ (circuitMain opts []) =<< Goldreich.makePRG
            "ggm"           -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGM
            "ggmSigma"      -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGMSigma
            "ggmNoPrg"      -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGMNoPrg
            "ggmNoPrgSigma" -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGMNoPrg
            "applebaum"     -> mapM_ (circuitMain opts []) =<< AR.makeApplebaum
            "tribes"        -> mapM_ (circuitMain opts []) =<< Tribes.make
            "point"         -> mapM_ (circuitMain opts []) =<< Point.make
            "comparison"    -> mapM_ (circuitMain opts []) =<< Comparison.make
            "ggmSigma256"   -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGMSigma256
            "ggmSigma1024"  -> mapM_ (circuitMain opts []) =<< Goldreich.makeGGMSigma1024
            "size_test"     -> mapM_ (circuitMain opts []) =<< Garbler.makeSizeTest
            _ -> do
                putStrLn "[error] known circuit generation modes: aes, aes1r, aes10r, goldreich, ggm,\
                        \ ggmSigma, ggmSigma256, ggmSigma1024, ggmNoPrg, ggmNoPrgSigma, applebaum,\
                        \ tribes, gf28Mult, point, comparison, size_test"
                exitFailure
    else
        processCircuit opts

circuitMain :: (Graphviz.Graphviz g, Optimize g, Gate g, ToAcirc g, ToCirc g, ToAcirc2 g)
            => Opts -> [TestCase] -> (Maybe FilePath, Circuit g) -> IO ()
circuitMain opts ts (outputName, c) = do
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

circuitMain' :: (Graphviz.Graphviz g, Optimize g, Gate g, ToAcirc g, ToCirc g, ToAcirc2 g)
            => Opts -> (Circuit g, [TestCase]) -> IO ()
circuitMain' opts (c,ts) = circuitMain opts ts (target opts, c)

processCircuit :: Opts -> IO ()
processCircuit opts = do
    let inp = source opts
        ext = takeExtension inp

    when (ext `notElem` [".acirc", ".nigel", ".netlist"]) $
        error (printf "[main] unknown input extension \"%s\"!" ext)

    case target opts of
        Nothing -> case ext of
            ".acirc" -> case coerce opts of
                Just "a"  -> circuitMain' opts =<< Acirc.readAcirc inp
                Just "a2" -> circuitMain' opts =<< over _1 toAcirc2 <$> Acirc.readAcirc inp
                Just _    -> error "[main] supported types for acirc: [a,a2]"
                Nothing   -> circuitMain' opts =<< Acirc.readAcirc inp
            ".nigel" -> case coerce opts of
                Just "a"   -> circuitMain' opts =<< (Nigel.readNigel inp :: IO (Acirc, [TestCase]))
                Just "a2"  -> circuitMain' opts =<< (Nigel.readNigel inp :: IO (Acirc2, [TestCase]))
                Just "b"   -> circuitMain' opts =<< (Nigel.readNigel inp :: IO (Acirc2, [TestCase]))
                Nothing    -> circuitMain' opts =<< (Nigel.readNigel inp :: IO (Circ, [TestCase]))
                Just other -> error "[main] supported coersion types for nigel: [a, a2, b]"
            ".netlist" -> case coerce opts of
                Just "a"   -> circuitMain' opts =<< (Netlist.readNetlist inp :: IO (Acirc, [TestCase]))
                Just "a2"  -> circuitMain' opts =<< (Netlist.readNetlist inp :: IO (Acirc2, [TestCase]))
                Just "b"   -> circuitMain' opts =<< (Netlist.readNetlist inp :: IO (Circ, [TestCase]))
                Nothing    -> circuitMain' opts =<< (Netlist.readNetlist inp :: IO (Circ, [TestCase]))
                Just other -> error "[main] supported coersion types for netlist: [a, a2, b]"

        Just outputFile -> case takeExtension outputFile of
            ".acirc" -> case ext of
                ".acirc"   -> circuitMain' opts =<< Acirc.readAcirc inp
                ".nigel"   -> circuitMain' opts =<< (Nigel.readNigel inp :: IO (Acirc, [TestCase]))
                ".netlist" -> circuitMain' opts =<< (Netlist.readNetlist inp :: IO (Acirc, [TestCase]))
            ".nigel" -> case ext of
                ".nigel"   -> circuitMain' opts =<< (Nigel.readNigel inp :: IO (Circ, [TestCase]))
                ".netlist" -> circuitMain' opts =<< (Netlist.readNetlist inp :: IO (Circ, [TestCase]))
                other      -> error "[main] supported input formats for nigel output: [nigel, netlist]"
            ".netlist" -> case ext of
                ".nigel"   -> circuitMain' opts =<< (Nigel.readNigel inp :: IO (Circ, [TestCase]))
                ".netlist" -> circuitMain' opts =<< (Netlist.readNetlist inp :: IO (Circ, [TestCase]))
                other      -> error "[main] supported input formats for netlist output: [nigel, netlist]"

evalTests :: Gate g => Opts -> Circuit g -> [TestCase] -> IO ()
evalTests opts c ts = do
    pr "evaluating plaintext circuit tests"
    ok <- ensure (verbose opts) c ts
    if ok then pr "ok" else pr "failed"

