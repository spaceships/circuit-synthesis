{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Text.Printf

data Mode = Garble { target :: FilePath
                   , naive  :: Bool
                   , opts   :: GlobalOpts
                   }
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
                      <> cmd "test"   evalTestParser "Evaluate the garbled circuit circuit then eval"
                      <> cmd "eval"   evalParser     "Evaluate a garbled circuit."

    garbleParser = Garble
                    <$> strArgument (metavar "CIRCUIT"
                                    <> help "The circuit to pruduce a circuit garbler for")
                    <*> switch (short 'n'
                               <> help "Whether to use the non-indexed garbler")
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
    Garble {..}   -> garble target naive opts
    EvalTest opts -> evalTest opts
    Eval opts     -> eval opts

-- produce a garbler for a circuit, then put everything in a nice directory for later
garble :: FilePath -> Bool -> GlobalOpts -> IO ()
garble fp naive opts = do
    when (verbose opts) $ do
        printf "reading %s\n" fp
    (c :: Circ2, ts) <- Circ.readWithTests fp

    when (print_info opts) $ do
        printf "info for %s as Circ2\n" fp
        printCircInfo c

    createDirectoryIfMissing False (directory opts)
    setCurrentDirectory (directory opts)

    when (verbose opts) $ do
        printf "creating garbler for %s " fp

    (gb, g2Desc) <- if naive
                       then do
                           when (verbose opts) (putStrLn "(naive)")
                           writeFile "naive" ""
                           naiveGarbler 1 c
                       else do
                           when (verbose opts) (putStrLn "(compact)")
                           removePathForcibly "naive"
                           indexedGarbler 1 1 c

    when (print_info opts) $ do
        printf "info for garbler\n"
        printCircInfo gb

    when (verbose opts) $ do
        putStrLn "writing gb.acirc2"
    Acirc2.write "gb.acirc2" gb

    when (verbose opts) $ do
        putStrLn "writing c.circ"
    Circ.write "c.circ" c

    when (verbose opts) $ do
        putStrLn "writing g2.txt"
    writeFile "g2.txt" g2Desc

evalTest :: GlobalOpts -> IO ()
evalTest opts = do
    setCurrentDirectory (directory opts)

    when (verbose opts) $ do
        putStrLn "reading gb.acirc2"

    naive <- doesFileExist "naive"

    undefined
    -- get outputs for all indices

eval :: GlobalOpts -> IO ()
eval = undefined

