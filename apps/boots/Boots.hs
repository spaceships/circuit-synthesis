{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Main where

import Boots.Garbler
import Boots.NaiveGarbler

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
                   , params   :: GarblerParams -- From Boots.Garbler
                   , opts     :: GlobalOpts
                   }
          | GenSeed SymId GlobalOpts
          | EvalTest GlobalOpts [String]
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
    cmd name parser desc = command name (info parser (progDesc desc))

    parser = hsubparser $ cmd "garble" garbleParser   "Produce a circuit for a garbler for a circuit."
                        <> cmd "seed"   seedParser     "Generate seeds."
                        <> cmd "test"   evalTestParser "Evaluate the garbled circuit circuit then eval."
                        <> cmd "eval"   evalParser     "Evaluate a garbled circuit."

    garbleParser = Garble
                    <$> strArgument (metavar "CIRCUIT"
                                    <> help "The circuit to pruduce a circuit garbler for")
                    <*> (GarblerParams
                        <$> option auto (short 's'
                                        <> help "Security parameter"
                                        <> showDefault <> value 40
                                        <> metavar "NUM")
                        <*> option auto (short 'p'
                                        <> help "Padding size"
                                        <> showDefault <> value 8
                                        <> metavar "SIZE")
                        <*> option auto (long "num-indices"
                                        <> help "Number of index sigma vectors"
                                        <> showDefault <> value 1
                                        <> metavar "NUM")
                        <*> option auto (short 'g' <> long "gates-per-index"
                                        <> help "Number of gates to compute per index"
                                        <> showDefault <> value 1
                                        <> metavar "NUM"))
                    <*> globalOptsParser

    seedParser = GenSeed
                    <$> (SymId <$> argument auto (metavar "SYM_NUM" <> help "the ciphertext number"))
                    <*> globalOptsParser

    evalTestParser = EvalTest
                    <$> globalOptsParser
                    <*> many (strArgument (metavar "INPUT" <> help "the input plaintexts"))

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
    g@(Garble {..}) -> garble g
    GenSeed sym opts -> genSeed sym opts
    EvalTest opts inps -> evalTest opts inps
    Eval opts -> eval opts

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

    when (verbose opts) $ printf "creating garbler for %s\n" target

    let naive = gatesPerIndex params >= length (garbleableGates c)

    (gb, (g0, g2)) <- if naive
                         then naiveGarbler params c
                         else garbler params c

    let wiresGen = genWiresGen params c g0

    when (verbose opts) $ putStr "naive: " >> print naive
    when naive $ writeFile "naive" ""

    when (verbose opts) $ putStrLn "writing params"
    writeFile "params" $ show params

    when (print_info opts) $ do
        printf "info for garbler\n"
        printCircInfo gb

    when (print_info opts) $ do
        printf "info for wires-gen.circ\n"
        printCircInfo wiresGen

    when (verbose opts) $ putStrLn "writing gb.acirc2"
    Acirc2.write "gb.acirc2" gb

    when (verbose opts) $ putStrLn "writing c.circ"
    Circ.write "c.circ" (toCirc c)

    when (verbose opts) $ putStrLn "writing wires-gen.circ"
    Acirc2.write "wires-gen.acirc2" wiresGen

    when (verbose opts) $ putStrLn "writing g2.circ"
    Circ.write "g2.circ" g2

genSeed :: SymId -> GlobalOpts -> IO ()
genSeed sym opts = do
    setCurrentDirectory (directory opts)

    when (verbose opts) $ putStrLn "reading params"
    params <- readParams

    let seedName = printf "seed%d" (getSymId sym)
    when (verbose opts) $ printf "generating %s\n" seedName
    seed <- randKeyIO (securityParam params)
    when (verbose opts) $ printf "%s = %s\n" seedName (showInts seed)
    when (verbose opts) $ printf "writing %s\n" seedName
    writeFile seedName (showInts seed)

evalTest :: GlobalOpts -> [String] -> IO ()
evalTest opts inps = do
    setCurrentDirectory (directory opts)

    when (verbose opts) $ putStrLn "reading params"
    params <- readParams

    when (verbose opts) $ putStrLn "reading c.circ"
    c <- Circ.read "c.circ" :: IO Circ2
    when (print_info opts) $ do
        printf "info for c.circ\n"
        printCircInfo c

    seeds <- forM [0..nsymbols c-1] $ \i -> do
        let seedName = printf "seed%d" i
        when (verbose opts) $ printf "reading %s\n" seedName
        readInts <$> readFile seedName

    when (verbose opts) $ putStrLn "reading wires-gen.circ"
    wiresGen <- Acirc2.read "wires-gen.acirc2" :: IO Acirc2
    when (print_info opts) $ do
        printf "info for wires-gen.acirc2\n"
        printCircInfo wiresGen

    when (length inps /= nsymbols c) $ do
        hPrintf stderr "[evalTest] circuit expects %d symbols, but got %d!\n"
            (nsymbols c) (length inps)
        exitFailure

    forM_ (zip [0..] inps) $ \(sym, inp) -> do
        when (length inp /= symlen c sym) $ do
            hPrintf stderr "[evalTest] symbol %d expects %d inputs, but got %d!\n"
                (getSymId sym) (symlen c sym) (length inp)
            exitFailure

    when (verbose opts) $ putStrLn "evaluating wires-gen.circ on seeds and inputs"
    let inputs = map readInts inps
        wires  = safeChunksOf (securityParam params) $
                    plainEval wiresGen (concat (interleave seeds inputs))

    when (verbose opts) $ putStrLn "writing wires"
    writeFile "wires" $ unlines (map showInts wires)

    when (verbose opts) $ putStrLn "reading gb.acirc2"
    gb <- Acirc2.read "gb.acirc2" :: IO Acirc2
    when (print_info opts) $ do
        printf "info for garbler\n"
        printCircInfo gb

    when (verbose opts) $ putStrLn "evaluating garbler on seeds"
    gs <-
        if gatesPerIndex params < length (garbleableGates c) then do
            when (verbose opts) $ putStrLn "generating every sigma vector combination"
            let indices i  = map (sigmaVector (symlen gb i)) [0..symlen gb i-1]
                allIndices = map concat $ sequence (map (indices.SymId) [nsymbols c..nsymbols gb - 1])
            forM allIndices $ \ix -> do
                return $ plainEval gb (concat seeds ++ ix)
        else do
            when (verbose opts) $ putStrLn "evaluating all gates at once"
            return $ [plainEval gb (concat seeds)]

    when (verbose opts) $ putStrLn "writing gates"
    writeFile "gates" (unlines (map showInts gs))

eval :: GlobalOpts -> IO ()
eval opts = do
    setCurrentDirectory (directory opts)

    when (verbose opts) $ putStrLn "reading params"
    params <- readParams

    when (verbose opts) $ putStrLn "reading c.circ"
    c <- Circ.read "c.circ" :: IO Circ2

    when (verbose opts) $ putStrLn "reading g2.circ"
    g2 <- Circ.read "g2.circ" :: IO Circ

    when (print_info opts) $ do
        printf "info for c.circ\n"
        printCircInfo c
        printf "info for g2.circ\n"
        printCircInfo g2

    when (verbose opts) $ putStrLn "reading wires"
    ws <- map readInts <$> lines <$> readFile "wires"

    let inputs  = listArray (0,InputId (ninputs c))   $ take (ninputs c) ws :: Array InputId [Int]
        consts  = listArray (0,ConstId (nconsts c))   $ take (nconsts c) (drop (ninputs c) ws)
        secrets = listArray (0,SecretId (nsecrets c)) $ take (nsecrets c) (drop (ninputs c + nconsts c) ws)

    when (verbose opts) $ putStrLn "reading gates"
    gs <- IM.fromList . zip (map (getRef.fst) (garbleableGates c)) <$>
          map readInts . safeChunksOf (4*(securityParam params + paddingSize params)) .
          concat . lines <$> readFile "gates"

    memo  <- newArray_ (0, Ref (nwires c)) :: IO (IOArray Ref [Int])
    stack <- newIORef []
    i     <- newIORef (Ref 0)

    let correctWire w = replicate (paddingSize params) 0 == take (paddingSize params) w

        correctOutputWire w = replicate (securityParam params-1) 0 == take (securityParam params-1) w

        backtrack ref = do -- pop stack, move i
            whenM (null <$> readIORef stack) $ do
                putStrLn "[backtrack: no refs to backtrack to!]"
                exitFailure
            (pos, val) <- head <$> readIORef stack
            when (verbose opts) $ do
                printf "[backtrack: popping stack to %d]\n" (getRef pos)
            modifyIORef stack tail
            writeIORef i pos
            return val

    whileM ((< nwires c) . getRef <$> readIORef i) $ do
        ref <- readIORef i
        let gate = getGate c ref

        when (verbose opts) $ do
            printf "[eval] gate %d: " (getRef ref)
            print gate

        val <- case gateGetBase gate of
            Just (Input  id) -> return $ inputs ! id
            Just (Const  id) -> return $ consts ! id
            Just (Secret id) -> return $ secrets ! id

            Nothing -> do
                [x,y] <- mapM (readArray memo) (gateArgs gate)

                case gate of
                    Bool2Xor _ _ | not (isOutputRef c ref) -> do
                        return $ zipWith xorInt x y

                    _ -> do
                        when (not (IM.member (getRef ref) gs)) $ do
                            printf "[eval] I tried to read the gate for ref %d, but it wasn't there!!!!\n" (getRef ref)
                            exitFailure

                        let opened  = openGate params g2 x y (gs IM.! getRef ref)
                            chunks  = safeChunksOf (securityParam params+paddingSize params) opened
                            choices = map (drop (paddingSize params)) (filter correctWire chunks)

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
                                    else if (length outputChoices > 1) then do
                                        putStrLn "[eval] multple output choices!!!! There must be a bug, yo."
                                        exitFailure
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

openGate :: GarblerParams -> Circ -> [Int] -> [Int] -> [Int] -> [Int]
openGate (GarblerParams {..}) g2 x y g = plainEval (opener g2) (x ++ y ++ g)
  where
    opener g2 = buildCircuit $ do
        x <- inputs securityParam
        y <- inputs securityParam
        zs <- replicateM 4 $ inputs (securityParam + paddingSize) -- garbled tables
        let g 0 x = take (securityParam+paddingSize) <$> subcircuit g2 x
            g 1 x = drop (securityParam+paddingSize) <$> subcircuit g2 x
        forM_ (zip zs (permutations 2 [0,1])) $ \(z, [i,j]) -> do
            gx <- g j x
            gy <- g i y
            gz <- foldM1 (zipWithM circXor) [gx, gy, z]
            outputs gz

--------------------------------------------------------------------------------

arr :: [a] -> Array Int a
arr xs = listArray (0,length xs-1) xs

readParams :: IO GarblerParams
readParams = read <$> readFile "params"
