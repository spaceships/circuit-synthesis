module Circuit.Format.Acirc
  ( Circuit.Format.Acirc.read
  , Circuit.Format.Acirc.write
  , readAcirc
  , writeAcirc
  , writeAcircR
  , parseCirc
  , showCirc
  , showTest
  , genTestStr
  , addTestsToFile
  , showCircWithTests
  ) where

import Circuit
import Types
import Circuit.Parser
import Util (addSpaces, forceM, readBits', showBits', safeInsert)

import Control.Monad
import qualified Control.Monad.State as S
import Text.Parsec hiding (spaces, parseTest)
import Text.Printf
import qualified Data.Set as S
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- printer

read :: FilePath -> IO Circuit
read = fmap fst . readAcirc

write :: FilePath -> Circuit -> IO ()
write = writeAcirc

showType :: MType -> String
showType Nothing  = ""
showType (Just t) = " @ " ++
  case t of
       Wire bt   -> show bt
       Vector bt -> "["  ++ show bt ++  "]"
       Matrix bt -> "[[" ++ show bt ++ "]]"

addTestsToFile :: FilePath -> IO ()
addTestsToFile fp = do
    s <- readFile fp
    let (c,_) = parseCirc s
    ts <- replicateM 10 (genTestStr 1 c)
    forceM ts
    writeFile fp (unlines ts ++ s)

writeAcirc :: FilePath -> Circuit -> IO ()
writeAcirc fp c = writeFile fp s
  where s = showCirc 1 c

writeAcircR :: FilePath -> Int -> Circuit -> IO ()
writeAcircR fp symlen c = do
    s <- showCircWithTestsR symlen 10 c
    writeFile fp s

showCircWithTests :: Int -> Circuit -> IO String
showCircWithTests = showCircWithTestsR 1

showCircWithTestsR :: Int -> Int -> Circuit -> IO String
showCircWithTestsR symlen ntests c = do
    ts <- replicateM ntests (genTestStr symlen c)
    return (unlines ts ++ showCirc symlen c)

showCirc :: Int -> Circuit -> String
showCirc symlen c = unlines (header ++ gateLines)
  where
    header = -- [printf ":nins %d" (ninputs c), printf ":depth %d" (depth c)] ++
             if symlen /= 1 then [printf ":symlen %d" symlen] else []

    inputs = mapM gateStr (circ_inputs c)
    consts = mapM gateStr (M.keys (circ_secret_refs c))
    gates  = mapM gateStr (nonInputGates c)

    output = do
        outs <- map show <$> mapM (tr . fst) (circ_outputs c)
        let outWithTypes = zipWith (++) outs (map showType (circ_output_type c))
        return [printf ":outputs %s" (unwords outs)]

    gateLines = concat $ S.evalState (sequence [inputs, consts, gates, output]) (M.empty, 0)

    -- we need to minimize the number of refs we use since we may not output them all
    tr :: Ref -> S.State (M.Map Ref Int, Int) Int
    tr ref = do
        (m,i) <- S.get
        case M.lookup ref m of
            Just j  -> return j
            Nothing -> do
                S.put (M.insert ref i m, i+1)
                return i

    gateStr :: Ref -> S.State (M.Map Ref Int, Int) String
    gateStr ref = do
        ref' <- tr ref
        case M.lookup ref (circ_refmap c) of
            Nothing -> error (printf "[gateStr] unknown ref %s" (show ref))
            Just ((OpInput  id t),_) -> return $ printf "%d input %d%s" ref' (getId id) (showType t)
            Just ((OpSecret id),_)   -> do
                let secret = case M.lookup id (circ_secrets c) of
                                Nothing -> ""
                                Just y  -> show y
                return $ printf "%d const %s" ref' secret
            Just ((OpSub xs),_)  -> prs ref' "-" xs
            Just ((OpMul xs),_)  -> prs ref' "*" xs
            Just ((OpNAdd xs),_) -> prs ref' "+" xs

    pr :: Int -> String -> Ref -> Ref -> S.State (M.Map Ref Int, Int) String
    pr ref' gateTy x y = do
        x' <- tr x
        y' <- tr y
        return $ printf "%d %s %d %d" ref' gateTy x' y'

    prs :: Int -> String -> [Ref] -> S.State (M.Map Ref Int, Int) String
    prs ref' gateTy xs = do
        xs' <- mapM tr xs
        let summands = concat $ fmap (printf " %d") xs' :: String
        return $ printf "%d %s%s" ref' gateTy summands

showTest :: TestCase -> String
showTest (inp, out) = printf ":test %s = %s" (addSpaces $ showBits' $ reverse inp) (addSpaces $ showBits' $ reverse out)

genTestStr :: Int -> Circuit -> IO String
genTestStr symlen c = fmap showTest (genTest symlen c)

--------------------------------------------------------------------------------
-- parser

readAcirc :: FilePath -> IO (Circuit, [TestCase])
readAcirc fp = parseCirc <$> readFile fp

parseCirc :: String -> (Circuit, [TestCase])
parseCirc s = case runParser (circParser >> getState) emptySt "" s of
    Left err -> error (show err)
    Right st -> (st_circ st, reverse (st_tests st))
  where
    circParser = optional parseVersion >> preamble >> lines >> end >> eof
    preamble = many $ (char ':' >> (parseTest <|> parseParam))
    lines    = many parseRefLine
    end      = parseOutputs

parseVersion :: ParseCirc Version
parseVersion = do
    char 'v'
    ma <- many1 digit
    char '.'
    mi <- many1 digit
    let v = Prelude.read ma :.: Prelude.read mi
    modifyState (\st -> st { st_ver = v })
    endLine
    return v

parseParam :: ParseCirc ()
parseParam = do
    skipMany (oneOf " \t" <|> alphaNum)
    endLine

parseTest :: ParseCirc ()
parseTest = do
    string "test"
    spaces
    inps <- many (oneOf "01")
    spaces
    outs <- many (oneOf "01")
    let inp = readBits' inps
        res = readBits' outs
    addTest (reverse inp, reverse res)
    endLine

parseOutputs :: ParseCirc ()
parseOutputs = do
    string ":outputs"
    spaces
    refs <- many (do ref <- parseRef; spaces; return ref)
    mapM_ markOutput refs
    endLine

parseRef :: ParseCirc Ref
parseRef = Ref <$> Prelude.read <$> many1 digit

parseRefLine :: ParseCirc ()
parseRefLine = do
    ref <- parseRef
    spaces
    choice [parseConst ref, parseInput ref, parseGate ref]
    endLine

parseBaseType :: ParseCirc BaseType
parseBaseType = (string "Integer" *> return Integer) <|> (string "Rational" *> return Rational)

parseType :: ParseCirc Type
parseType =  (parseBaseType >>= return . Wire)
         <|> (char '[' *> (parseVector <|> parseMatrix) <* char ']')
  where
    parseVector = parseBaseType >>= return . Vector
    parseMatrix = char '[' *> (parseBaseType >>= return . Matrix) <* char ']'

parseInput :: Ref -> ParseCirc ()
parseInput ref = do
    string "input"
    spaces
    id <- Id <$> Prelude.read <$> many1 digit
    v <- getVersion
    case v == vZero of
         True  -> insertInput ref id
         False -> do
            spaces
            char '@'
            spaces
            t <- parseType
            insertInputType ref id (Just t)

parseConst :: Ref -> ParseCirc ()
parseConst ref = do
    string "const"
    spaces
    val <- Prelude.read <$> many1 digit
    id  <- nextConstId
    insertSecret ref id
    insertSecretVal id val

parseGate :: Ref -> ParseCirc ()
parseGate ref = do
    -- gateType <- oneOfStr ["gate", "output"]
    -- when (gateType == "output") $ markOutput ref
    opType <- oneOfStr ["+", "-", "*"]
    spaces
    -- xref <- Ref <$> read <$> ((:) <$> option ' ' (char '-') <*> many1 digit)
    -- spaces
    -- yref <- Ref <$> read <$> ((:) <$> option ' ' (char '-') <*> many1 digit)
    let refP = Ref <$> Prelude.read <$> many1 digit
    xrefs <- refP `sepBy1` spaces
    let op = case opType of
            "+" -> OpNAdd xrefs
            "*" -> OpMul xrefs
            "-" -> OpSub xrefs
            g     -> error ("[parser] unkonwn gate type " ++ g)
    insertOp ref op
