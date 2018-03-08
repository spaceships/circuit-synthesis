{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Circuit.Format.Circ where

import Circuit
import Circuit.Utils
import Circuit.Parser
import qualified Circuit.Builder as B

import Control.Monad
import Control.Monad.Trans
import Text.Parsec hiding (spaces, parseTest, parse)
import Data.Maybe
import Formatting ((%))
import Lens.Micro.Platform
import TextShow
import qualified Data.IntMap as IM
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Formatting as F
import qualified Data.IntSet as IS

data CircSt = CircSt { _circ_st_outputs     :: [Ref] -- old refs from the previous circuit
                     , _circ_st_const_vals  :: IM.IntMap Int
                     , _circ_st_secret_vals :: IM.IntMap Int
                     , _circ_st_tr          :: IM.IntMap Ref
                     }

makeLenses ''CircSt

emptyCircSt = CircSt [] IM.empty IM.empty IM.empty

type CircParser g = ParseCirc g CircSt

--------------------------------------------------------------------------------

read :: Gate g => FilePath -> IO (Circuit g)
read = fmap fst . readWithTests

readWithTests :: Gate g => FilePath -> IO (Circuit g, [TestCase])
readWithTests fp = parse <$> readFile fp

write :: FilePath -> Circ -> IO ()
write fp c = T.writeFile fp (showWithTests c [])

writeWithTests :: FilePath -> Circ -> IO ()
writeWithTests fp c = do
    ts <- replicateM 10 (genTest c)
    T.writeFile fp (showWithTests c ts)

showWithTests :: Circ -> [TestCase] -> T.Text
showWithTests !c !ts = T.unlines (header ++ inputs ++ secrets ++ consts ++ gates)
  where
    header = [ T.append ":ninputs " (showt (ninputs c))
             , T.append ":outputs " (T.unwords (map (showt.getRef) (outputRefs c)))
             , T.append ":symlens " (T.unwords (map showt (c^..circ_symlen.each)))
             , T.append ":sigmas "  (T.unwords (map (showt . (b2i :: Bool -> Int)
                                    . flip IS.member (c^.circ_sigma_vecs)) [0..nsymbols c-1]))
             , T.append ":nrefs "   (showt (c ^. circ_maxref))
             , T.append ":consts "  (T.unwords (map showt (IM.elems (c^.circ_const_vals))))
             , T.append ":secrets " (T.unwords (map showt (IM.elems (c^.circ_secret_vals))))
             ] ++ map showTest ts
               ++ [ ":start" ]

    inputs  = mapMaybe gateTxt (inputRefs c)
    consts  = mapMaybe gateTxt (constRefs c)
    secrets = mapMaybe gateTxt (secretRefs c)
    gates   = mapMaybe gateTxt (gateRefs c)

    gateTxt :: Ref -> Maybe T.Text
    gateTxt !ref =
        case c ^. circ_refcount . at (getRef ref) of
            Nothing -> Nothing
            Just ct -> Just $ case c ^. circ_refmap . at (getRef ref) . non (error "[gateTxt] unknown ref") of
                (BoolBase (Input  id)) -> F.sformat (F.shown % " input "  % F.shown) ref id
                (BoolBase (Const  id)) -> F.sformat (F.shown % " const "  % F.shown) ref id
                (BoolBase (Secret id)) -> F.sformat (F.shown % " secret " % F.shown) ref id
                (BoolXor x y) -> F.sformat (F.shown % " xor " % F.shown % " " % F.shown) ref x y
                (BoolAnd x y) -> F.sformat (F.shown % " and " % F.shown % " " % F.shown) ref x y
                (BoolNot x)   -> F.sformat (F.shown % " not " % F.shown) ref x

showTest :: TestCase -> T.Text
showTest (!inp, !out) = T.concat [":test ", T.pack (showInts inp), " ", T.pack (showInts out) ]

--------------------------------------------------------------------------------

parse :: Gate g => String -> (Circuit g, [TestCase])
parse s = runCircParser emptyCircSt parser s
  where
    parser = do
        many $ char ':' >> choice [parseTest, parseOutputs, parseConsts, try parseSecrets, try parseSymlen, try parseSigmas, skipParam]
        many parseRefLine
        lift . B.outputs =<< mapM tr =<< _circ_st_outputs <$> getSt -- translate the outputs
        eof

tr :: Ref -> CircParser g Ref
tr x = (IM.! getRef x) . _circ_st_tr <$> getSt

skipParam :: CircParser g ()
skipParam = do
    skipMany (oneOf " \t" <|> alphaNum)
    endLine

parseTest :: CircParser g ()
parseTest = do
    string "test"
    spaces
    inps <- many digit
    spaces
    outs <- many digit
    let inp = readInts inps
        res = readInts outs
    addTest (inp, res)
    endLine

parseOutputs :: CircParser g ()
parseOutputs = do
    string "outputs"
    spaces
    refs <- many (parseRef <* spaces)
    modifySt (circ_st_outputs .~ refs)
    endLine

parseSecrets :: CircParser g ()
parseSecrets = do
    string "secrets"
    spaces
    secs <- many (int <* spaces)
    modifySt (circ_st_secret_vals .~ IM.fromList (zip [0..] secs))
    endLine

parseConsts :: CircParser g ()
parseConsts = do
    string "consts"
    spaces
    cs <- many (spaces >> int)
    modifySt (circ_st_const_vals .~ IM.fromList (zip [0..] cs))
    endLine

parseRef :: CircParser g Ref
parseRef = Ref <$> int

parseRefLine :: Gate g => CircParser g ()
parseRefLine = do
    z <- parseRef
    spaces
    z' <- choice [parseConst, parseSecret, parseInput, parseNot, parseAnd, parseXor]
    modifySt (circ_st_tr . at (getRef z) ?~ z')
    endLine

parseInput :: Gate g => CircParser g Ref
parseInput = do
    string "input"
    spaces
    id <- InputId <$> int
    lift $ B.inputBitN id

parseConst :: Gate g => CircParser g Ref
parseConst = do
    string "const"
    spaces
    id  <- int
    val <- (IM.! id) . _circ_st_const_vals <$> getSt
    lift $ B.constant val

parseSymlen :: Gate g => CircParser g ()
parseSymlen = do
    string "symlens"
    spaces
    symlens <- many (spaces >> int)
    lift $ zipWithM B.setSymlen [0::SymId ..] symlens
    endLine

parseSigmas :: CircParser g ()
parseSigmas = do
    string "sigmas"
    spaces
    sigs <- many (spaces >> i2b <$> int)
    lift $ forM (zip [0::SymId ..] sigs) $ \(id, sig) -> do
        when sig (B.setSigma id)
    endLine

parseSecret :: Gate g => CircParser g Ref
parseSecret = do
    string "secret"
    spaces
    id  <- int
    val <- (IM.! id) . _circ_st_secret_vals <$> getSt
    lift $ B.secret val

parseNot :: Gate g => CircParser g Ref
parseNot = do
    string "not"
    spaces
    x <- tr =<< parseRef
    lift $ B.circNot x

parseAnd :: Gate g => CircParser g Ref
parseAnd = do
    string "and"
    spaces
    x <- tr =<< parseRef
    spaces
    y <- tr =<< parseRef
    lift $ B.circAnd x y

parseXor :: Gate g => CircParser g Ref
parseXor = do
    string "xor"
    spaces
    x <- tr =<< parseRef
    spaces
    y <- tr =<< parseRef
    lift $ B.circXor x y
