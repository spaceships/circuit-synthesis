{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Main where

import Circuit
import Circuit.Conversion
import Circuit.Optimizer
import Circuit.Utils
import Examples.Garbler
import qualified Circuit.Format.Acirc2 as Acirc2
import qualified Circuit.Format.Circ as Circ

import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative
import System.Directory
import System.Exit
import Text.Printf

data Mode = Garble { target :: FilePath
                   , naive  :: Bool
                   , opts   :: GlobalOpts
                   }
          | GenWires String GlobalOpts
          | EvalTest GlobalOpts
          | Eval GlobalOpts
          deriving (Show)

data GlobalOpts = GlobalOpts
                { directory  :: String
                , print_info :: Bool
                , verbose    :: Bool
                } deriving (Show)


parseArgs :: IO Mode
parseArgs = execParser $ info (parser <**> helper)
    (fullDesc <> progDesc "boots is a program to generate a circuit for a PRG \
                          \ garbler, and also to evaluate it")
  where
    cmd name parser desc = command name (info (parser <**> helper) (progDesc desc))

    parser = subparser $ cmd "garble" garbleParser   "Produce a circuit for a garbler for a circuit."
                      <> cmd "wires"  wiresParser    "Generate seed and input wirelabels."
                      <> cmd "test"   evalTestParser "Evaluate the garbled circuit circuit then eval."
                      <> cmd "eval"   evalParser     "Evaluate a garbled circuit."

    garbleParser = Garble
                    <$> strArgument (metavar "CIRCUIT"
                                    <> help "The circuit to pruduce a circuit garbler for")
                    <*> switch (short 'n'
                               <> help "Whether to use the non-indexed garbler")
                    <*> globalOptsParser

    wiresParser = GenWires
                    <$> strArgument (metavar "INPUT" <> help "the input to translate to wirelabels")
                    <*> globalOptsParser

    evalTestParser = EvalTest <$> globalOptsParser

    evalParser = Eval <$> globalOptsParser

    globalOptsParser = GlobalOpts
                    <$> strOption
                        (short 'd'
                        <> metavar "DIR"
                        <> showDefault
                        <> value "obf"
                        <> help "Use DIR as the base directory for the obfuscation")
                    <*> switch (short 'i' <> help "Show circuit info")
                    <*> switch (short 'v' <> help "Set verbose mode")

main = parseArgs >>= \case
    Garble {..}       -> garble target naive opts
    GenWires inp opts -> genWires inp opts
    EvalTest opts     -> evalTest opts
    Eval opts         -> eval opts

-- produce a garbler for a circuit, then put everything in a nice directory for later
garble :: FilePath -> Bool -> GlobalOpts -> IO ()
garble fp naive opts = do
    when (verbose opts) $ printf "reading %s\n" fp
    (c :: Circ2, ts) <- Circ.readWithTests fp

    when (print_info opts) $ do
        printf "info for %s as Circ2\n" fp
        printCircInfo c

    createDirectoryIfMissing False (directory opts)
    setCurrentDirectory (directory opts)

    when (verbose opts) $ printf "creating garbler for %s " fp
    (gb, (g1, g2)) <- if naive
        then do
            when (verbose opts) $ putStrLn "(naive)"
            writeFile "naive" ""
            naiveGarbler 1 c
        else do
            when (verbose opts) $ putStrLn "(compact)"
            removePathForcibly "naive"
            indexedGarbler 1 1 c

    when (print_info opts) $ do
        printf "info for garbler\n"
        printCircInfo gb

    when (verbose opts) $ putStrLn "writing gb.acirc2"
    Acirc2.write "gb.acirc2" gb

    when (verbose opts) $ putStrLn "writing c.circ"
    Circ.write "c.circ" c

    when (verbose opts) $ putStrLn "writing g1.circ"
    Circ.write "g1.circ" g1

    when (verbose opts) $ putStrLn "writing g2.circ"
    Circ.write "g2.circ" g2

    when (verbose opts) $ putStrLn "ok"

-- TODO: give out const and secret wirelabels here too
genWires :: String -> GlobalOpts -> IO ()
genWires inpStr opts = do
    setCurrentDirectory (directory opts)

    when (verbose opts) $ putStrLn "reading c.circ"
    c <- Circ.read "c.circ" :: IO Circ

    when (length inpStr /= ninputs c) $ do
        printf "[genWires] circuit expects %d inputs, but got %d!\n"
            (ninputs c) (length inpStr)
        exitFailure

    when (verbose opts) $ putStrLn "reading g1.circ"
    g1 <- Circ.read "g1.circ" :: IO Circ

    when (verbose opts) $ putStrLn "generating seed"
    seed <- randKeyIO 80
    when (verbose opts) $ printf "seed = %s\n" (showInts seed)

    when (verbose opts) $ putStrLn "writing seed"
    writeFile "seed" (showInts seed)

    when (verbose opts) $ putStrLn "evaluating g1 on seed"
    let wirePairs = pairsOf $ safeChunksOf 80 $ plainEval g1 seed

    when (verbose opts) $ putStrLn "writing wires"
    let choose False = fst
        choose True  = snd
        wires = zipWith choose (readBits' inpStr) wirePairs
    writeFile "wires" (unlines (map showInts wires))

    when (verbose opts) $ putStrLn "ok"

evalTest :: GlobalOpts -> IO ()
evalTest opts = do
    setCurrentDirectory (directory opts)

    when (verbose opts) $ putStrLn "reading seed"
    seed <- readInts <$> readFile "seed"

    when (verbose opts) $ putStrLn "reading c.circ"
    c <- Circ.read "c.circ" :: IO Circ

    when (print_info opts) $ do
        printf "info for c.circ\n"
        printCircInfo c

    when (verbose opts) $ putStrLn "reading gb.acirc2"
    gb <- Acirc2.read "gb.acirc2" :: IO Acirc2

    when (print_info opts) $ do
        printf "info for garbler\n"
        printCircInfo gb

    when (verbose opts) $ putStrLn "evaluating garbler"
    naive <- doesFileExist "naive"
    let gs = if naive
                then safeChunksOf 90 (plainEval gb seed)
                else let indices i  = map (sigmaVector (symlen gb i)) [0..symlen gb i-1]
                         --  every sigma vector combination
                         allIndices = map concat $ sequence (map indices [1..nsymbols gb - 1])
                         garble  ix = plainEval gb (seed ++ ix)
                     -- XXX: probably going to want a progress bar here eventually
                     in map garble allIndices -- outputs for all indices

    when (verbose opts) $ putStrLn "writing gates"
    writeFile "gates" (unlines (map showInts gs))

eval :: GlobalOpts -> IO ()
eval = undefined
    -- when (verbose opts) $ putStrLn "ok"
