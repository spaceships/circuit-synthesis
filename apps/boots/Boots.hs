{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Main where

import Boots.Garbler

import Circuit
import Circuit.Builder
import Circuit.Conversion
import Circuit.Optimizer
import Circuit.Utils
import qualified Circuit.Format.Acirc    as Acirc
import qualified Circuit.Format.Acirc2   as Acirc2
import qualified Circuit.Format.Bench    as Bench
import qualified Circuit.Format.Circ     as Circ
import qualified Circuit.Format.Nigel    as Nigel
import qualified Circuit.Format.Netlist  as Netlist
import qualified Circuit.Format.Graphviz as Graphviz

import Control.Monad
import Control.Monad.Loops
import Data.Array
import Data.Array.IO
import Data.IORef
import Data.Semigroup ((<>))
import Options.Applicative hiding (Const)
import System.Directory
import System.Exit
import System.FilePath.Posix (takeBaseName, takeExtension)
import System.IO
import Text.Printf
import qualified Data.IntMap as IM

--------------------------------------------------------------------------------
-- command line options

data Mode = Garble { target   :: FilePath
                   , naive    :: Bool
                   , indexed  :: Bool
                   , security :: Int
                   , padding  :: Int
                   , opts     :: GlobalOpts
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
                    <*> switch (short 'n' <> help "Whether to use the naive garbler")
                    <*> switch (short 'x' <> help "Whether to use the indexed non-free-xor garbler")
                    <*> option auto (short 'p' <> help "Padding size" <> showDefault <> metavar "SIZE" <> value 4)
                    <*> option auto (short 's' <> help "Security parameter" <> showDefault <> metavar "NUM" <> value 80)
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
    g@(Garble _ _ _ _ _ _) -> garble g
    GenWires inp opts -> genWires inp opts
    EvalTest opts     -> evalTest opts
    Eval opts         -> eval opts

--------------------------------------------------------------------------------
-- protocol implementations

readFormat :: Gate g => FilePath -> IO (Circuit g, [TestCase])
readFormat inp = case takeExtension inp of
    ".acirc"   -> Acirc.readWithTests inp
    ".acirc2"  -> Acirc2.readWithTests inp
    ".circ"    -> Circ.readWithTests inp
    ".nigel"   -> Nigel.readNigel inp
    ".netlist" -> Netlist.readNetlist inp
    ".bench"   -> Bench.readBench inp
    other      -> error (printf "[readFormat] unsupported input extension: \"%s\"!" other)

-- produce a garbler for a circuit, then put everything in a nice directory for later
garble :: Mode -> IO ()
garble (Garble {..}) = do
    when (verbose opts) $ printf "reading %s\n" target
    (c :: Circ2, ts) <- readFormat target

    when (print_info opts) $ do
        printf "info for %s as Circ2\n" target
        printCircInfo c

    createDirectoryIfMissing False (directory opts)
    setCurrentDirectory (directory opts)

    when (verbose opts) $ printf "creating garbler for %s " target
    (gb, (g1, g2)) <-
        if naive then do
            when (verbose opts) $ putStrLn "(naive)"
            writeFile "naive" ""
            removePathForcibly "not-free-xor"
            naiveGarbler security padding 1 c
        else if indexed then do
            when (verbose opts) $ putStrLn "(indexed)"
            writeFile "not-free-xor" ""
            removePathForcibly "naive"
            indexedGarbler security padding 1 c
        else do
            when (verbose opts) $ putStrLn "(freeXOR indexed)"
            removePathForcibly "not-free-xor"
            removePathForcibly "naive"
            indexedGarblerFreeXor security padding 1 c

    when (verbose opts) $ putStrLn "writing params"
    writeFile "params" $ unlines [show security, show padding]

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

genWires :: String -> GlobalOpts -> IO ()
genWires inpStr opts = do
    setCurrentDirectory (directory opts)

    when (verbose opts) $ putStrLn "reading params"
    (security, padding) <- readParams

    when (verbose opts) $ putStrLn "reading c.circ"
    c <- Circ.read "c.circ" :: IO Circ

    when (length inpStr /= ninputs c) $ do
        hPrintf stderr "[genWires] circuit expects %d inputs, but got %d!\n"
            (ninputs c) (length inpStr)
        exitFailure

    when (verbose opts) $ putStrLn "reading g1.circ"
    g1 <- Circ.read "g1.circ" :: IO Circ


    when (verbose opts) $ putStrLn "generating seed"
    seed <- randKeyIO security
    when (verbose opts) $ printf "seed = %s\n" (showInts seed)

    when (verbose opts) $ putStrLn "writing seed"
    writeFile "seed" (showInts seed)

    notFree <- doesFileExist "not-free-xor"

    when (verbose opts) $ putStrLn "evaluating g1 on seed"
    wirePairs <- if notFree then do
            let raw   = plainEval g1 seed
                pairs = pairsOf $ safeChunksOf security raw
            return $ listArray (Ref 0, Ref (nwires c-1)) pairs
        else do
            let (delta:falseWLs) = safeChunksOf security $ plainEval g1 seed
                trueWLs = map (zipWith xorInt delta) falseWLs
            return $ listArray (Ref 0, Ref (nwires c-1)) (zip falseWLs trueWLs)

    when (verbose opts) $ putStrLn "writing wires"
    let inputWires  = zipWith choosePair (readInts inpStr) (map (wirePairs !) (inputRefs c))
        constWires  = zipWith choosePair (constVals c)     (map (wirePairs !) (constRefs c))
        secretWires = zipWith choosePair (secretVals c)    (map (wirePairs !) (secretRefs c))
        wires = inputWires ++ constWires ++ secretWires
    writeFile "wires" $ unlines (map showInts wires)

    when (verbose opts) $ putStrLn "ok"

evalTest :: GlobalOpts -> IO ()
evalTest opts = do
    setCurrentDirectory (directory opts)

    when (verbose opts) $ putStrLn "reading params"
    (security, padding) <- readParams

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

    when (verbose opts) $ putStr "evaluating garbler "
    naive <- doesFileExist "naive"
    gs <- if naive
            then do
                when (verbose opts) $ putStrLn "(naive)"
                return $ safeChunksOf (4*(security+padding)) (plainEval gb seed)
            else do
                when (verbose opts) $ putStrLn "(compact)"
                --  generate every sigma vector combination
                let indices i  = map (sigmaVector (symlen gb i)) [0..symlen gb i-1]
                    allIndices = map concat $ sequence (map indices [1..SymId (nsymbols gb - 1)])

                forM (zip [1..] allIndices) $ \(done, ix) -> do
                    return $ plainEval gb (seed ++ ix)

    when (verbose opts) $ putStrLn "writing gates"
    writeFile "gates" (unlines (map showInts gs))

eval :: GlobalOpts -> IO ()
eval opts = do
    setCurrentDirectory (directory opts)

    when (verbose opts) $ putStrLn "reading params"
    (security, padding) <- readParams

    when (verbose opts) $ putStrLn "reading c.circ"
    c <- Circ.read "c.circ" :: IO Circ2

    when (verbose opts) $ putStrLn "reading g2.circ"
    g2 <- Circ.read "g2.circ" :: IO Circ

    when (print_info opts) $ do
        printf "info for c.circ\n"
        printCircInfo c
        printf "info for g2.circ\n"
        printCircInfo g2

    notFree <- doesFileExist "not-free-xor"

    when (verbose opts) $ putStrLn "reading wires"
    ws <- map readInts <$> lines <$> readFile "wires"

    let relevantGateRefs = map fst $ if notFree
                                        then gates c
                                        else filter (not.isXor.snd) (gates c)

    when (verbose opts) $ putStrLn "reading gates"
    gs <- IM.fromList . zip (map getRef relevantGateRefs) <$>
          map readInts . lines <$> readFile "gates"

    let inputs  = listArray (0,InputId (ninputs c))   $ take (ninputs c) ws :: Array InputId [Int]
        consts  = listArray (0,ConstId (nconsts c))   $ take (nconsts c) (drop (ninputs c) ws)
        secrets = listArray (0,SecretId (nsecrets c)) $ take (nsecrets c) (drop (ninputs c + nconsts c) ws)

    memo  <- newArray_ (0, Ref (nwires c)) :: IO (IOArray Ref [Int])
    stack <- newIORef []
    i     <- newIORef (Ref 0)

    let correctOutputWire w = replicate (security-1) 0 == take (security-1) w

        backtrack ref = do -- pop stack, move i
            failed <- null <$> readIORef stack
            when failed $ do
                printf "[error] failed to decrypt ref %d! no refs to backtrack to!\n" (getRef ref)
                exitFailure
            (pos, val) <- head <$> readIORef stack
            when (verbose opts) $ do
                printf "[ref %d failed: popping stack to %d]\n" (getRef ref) (getRef pos)
            modifyIORef stack tail
            writeIORef i pos
            return val

    whileM ((< nwires c) . getRef <$> readIORef i) $ do
        ref <- readIORef i
        let gate = getGate c ref

        when (verbose opts) $ do
            printf "[eval] gate %d:" (getRef ref)
            print gate

        val <- case gateGetBase gate of
            Just (Input  id) -> return $ inputs ! id
            Just (Const  id) -> return $ consts ! id
            Just (Secret id) -> return $ secrets ! id

            Nothing -> do
                [x,y] <- mapM (readArray memo) (gateArgs gate)

                case (notFree, gate) of
                    (False, Bool2Xor _ _) -> return $ zipWith xorInt x y

                    _ -> do
                        let opened  = openGate security padding g2 x y (gs IM.! getRef ref)
                            chunks  = safeChunksOf (security+padding) opened
                            choices = map (drop padding) $
                                      filter ((== replicate padding 0). take padding) chunks

                        when (verbose opts) $ do
                            mapM_ (putStrLn.showInts) chunks

                        case choices of
                            []  -> backtrack ref
                            [w] -> if isOutputRef c ref && not (correctOutputWire w)
                                      then backtrack ref
                                      else return w

                            (w:ws) -> do -- push stack, try first choice
                                if isOutputRef c ref then do
                                    let outputChoices = filter correctOutputWire (w:ws)
                                    if (length outputChoices == 1) then do
                                        return (head outputChoices)
                                    else do
                                        -- backtrack
                                        when (verbose opts) $
                                            printf "[output ref %d failed: backtracking]\n" (getRef ref)
                                        backtrack ref
                                else do
                                    when (verbose opts) $ do
                                        printf "[ref %d multiple: %d alternates]\n" (getRef ref) (length ws + 1)
                                    mapM_ (\val -> modifyIORef stack ((ref,val):)) ws
                                    return w

        ref' <- readIORef i -- potentially changed! (this was a fun bug to find)
        writeArray memo ref' val
        modifyIORef i (+1)

    res <- mapM (readArray memo) (outputRefs c)

    when (verbose opts) $ do
        putStr "output wirelabel: "
        mapM_ (putStrLn.showInts) res

    putStrLn $ unwords (map (show.last) res)

    when (verbose opts) $ putStrLn "ok"

openGate :: Int -> Int -> Circ -> [Int] -> [Int] -> [Int] -> [Int]
openGate security padding g2 x y g = plainEval (opener g2) (x ++ y ++ g)
  where
    opener g2 = buildCircuit $ do
        x <- inputs security
        y <- inputs security
        zs <- replicateM 4 $ inputs (security + padding) -- garbled tables
        let g 0 x = take (security+padding) <$> subcircuit g2 x
            g 1 x = drop (security+padding) <$> subcircuit g2 x
        forM_ (zip zs (permutations 2 [0,1])) $ \(z, [i,j]) -> do
            gx <- g j x
            gy <- g i y
            gz <- foldM1 (zipWithM circXor) [gx, gy, z]
            outputs gz

--------------------------------------------------------------------------------

arr :: [a] -> Array Int a
arr xs = listArray (0,length xs-1) xs

readParams :: IO (Int, Int)
readParams = do
    [sec,pad] <- map read . lines <$> readFile "params"
    return (sec, pad)
