{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Circuit.Format.Acirc
  ( Circuit.Format.Acirc.read
  , Circuit.Format.Acirc.write
  , Circuit.Format.Acirc.show
  , writeWithTests
  , readWithTests
  , showWithTests
  , parse
  ) where

import Circuit
import Circuit.Parser
import Circuit.Utils hiding ((%))
import qualified Circuit.Builder as B
import qualified Circuit.Builder.Internals as B

import Prelude hiding (show)
import Control.Monad
import Control.Monad.Trans (lift)
import Data.Maybe (mapMaybe)
import Formatting ((%))
import Lens.Micro.Platform
import Text.Parsec hiding (spaces, parseTest, parse)
import TextShow
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import qualified Formatting as F

data AcircSt = AcircSt { _acirc_st_outputs     :: [Ref] -- old refs from the previous circuit
                       , _acirc_st_const_vals  :: IM.IntMap Int
                       , _acirc_st_secret_vals :: IM.IntMap Int
                       , _acirc_st_tr          :: IM.IntMap Ref
                       }

makeLenses ''AcircSt

emptyAcircSt = AcircSt [] IM.empty IM.empty IM.empty

type AcircParser g = ParseCirc g AcircSt

--------------------------------------------------------------------------------

read :: Gate g => FilePath -> IO (Circuit g)
read = fmap fst . readWithTests

readWithTests :: Gate g => FilePath -> IO (Circuit g, [TestCase])
readWithTests fp = parse <$> readFile fp

write :: FilePath -> Acirc -> IO ()
write fp c = T.writeFile fp (show c)

writeWithTests :: FilePath -> Acirc -> IO ()
writeWithTests fp c = do
    ts <- replicateM 10 (genTest c)
    T.writeFile fp (showWithTests c ts)

show :: Acirc -> T.Text
show c = showWithTests c []

showWithTests :: Acirc -> [TestCase] -> T.Text
showWithTests !c !ts = T.unlines (header ++ inputs ++ secrets ++ consts ++ gates)
  where
    header = [ T.append ":ninputs " (showt (ninputs c))
             , T.append ":nrefs "   (showt (c ^. circ_maxref))
             , T.append ":consts "  (T.unwords (map showt (IM.elems (_circ_const_vals c))))
             , T.append ":secrets " (T.unwords (map showt (IM.elems (_circ_secret_vals c))))
             , T.append ":outputs " (T.unwords (map (showt.getRef) (outputRefs c)))
             , T.append ":symlens " (T.unwords (map showt (c^..circ_symlen.each)))
             , T.append ":sigmas "  (T.unwords (map (showt . (b2i :: Bool -> Int)
                                    . flip IS.member (c^.circ_sigma_vecs)) [0..nsymbols c-1]))
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
            Just ct -> Just $ case c^.circ_refmap.at (getRef ref).non undefined of
                (ArithBase (Input  id)) -> F.sformat (F.shown % " input "  % F.shown % " : " % F.int) ref id ct
                (ArithBase (Const  id)) -> F.sformat (F.shown % " const "  % F.shown % " : " % F.int) ref id ct
                (ArithBase (Secret id)) -> F.sformat (F.shown % " secret " % F.shown % " : " % F.int) ref id ct
                (ArithAdd x y) -> F.sformat (F.shown % " add " % F.shown % " " % F.shown % " : " % F.int % F.stext) ref x y ct (saveStr ref)
                (ArithSub x y) -> F.sformat (F.shown % " sub " % F.shown % " " % F.shown % " : " % F.int % F.stext) ref x y ct (saveStr ref)
                (ArithMul x y) -> F.sformat (F.shown % " mul " % F.shown % " " % F.shown % " : " % F.int % F.stext) ref x y ct (saveStr ref)

    saveStr ref = if IS.member (getRef ref) (c^.circ_refsave)
                     then " save"
                     else if IS.member (getRef ref) (c^.circ_refskip)
                             then " skip"
                             else ""

showTest :: TestCase -> T.Text
showTest (!inp, !out) = T.concat [":test ", T.pack (showInts inp), " ", T.pack (showInts out) ]

--------------------------------------------------------------------------------
-- parser

parse :: Gate g => String -> (Circuit g, [TestCase])
parse s = runCircParser emptyAcircSt parser s
  where
    parser = do
        many $ char ':' >> choice [parseTest, parseOutputs, parseConsts, try parseSymlen,
                                   try parseSigma, try parseSecrets, skipParam]
        many parseRefLine
        lift . B.outputs =<< mapM tr =<< view acirc_st_outputs <$> getSt -- translate the outputs
        eof

tr :: Ref -> AcircParser g Ref
tr x = (IM.! getRef x) . view acirc_st_tr <$> getSt

skipParam :: AcircParser g ()
skipParam = do
    skipMany (oneOf " \t" <|> alphaNum)
    endLine

parseTest :: AcircParser g ()
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

parseOutputs :: AcircParser g ()
parseOutputs = do
    string "outputs"
    spaces
    refs <- many (parseRef <* spaces)
    modifySt (acirc_st_outputs .~ refs)
    endLine

parseSecrets :: AcircParser g ()
parseSecrets = do
    string "secrets"
    spaces
    secs <- many (int <* spaces)
    modifySt (acirc_st_secret_vals .~ IM.fromList (zip [0..] secs))
    endLine

parseConsts :: AcircParser g ()
parseConsts = do
    string "consts"
    spaces
    cs <- many (spaces >> int)
    modifySt (acirc_st_const_vals .~ IM.fromList (zip [0..] cs))
    endLine

parseSymlen :: AcircParser g ()
parseSymlen = do
    string "symlens"
    spaces
    symlens <- many (spaces >> int)
    lift $ zipWithM B.setSymlen [0::SymId ..] symlens
    endLine

parseSigma :: AcircParser g ()
parseSigma = do
    string "sigmas"
    spaces
    sigs <- many (spaces >> i2b <$> int)
    lift $ forM (zip [0::SymId ..] sigs) $ \(id, sig) -> do
        when sig (B.setSigma id)
    endLine

parseRef :: AcircParser g Ref
parseRef = Ref <$> int

parseRefLine :: Gate g => AcircParser g ()
parseRefLine = do
    z <- parseRef
    spaces
    z' <- choice [parseConst, try parseSecret, parseInput, parseAdd, parseSub, parseMul]
    modifySt (acirc_st_tr . at (getRef z) ?~ z')
    parseTimesUsed z'
    endLine

parseInput :: Gate g => AcircParser g Ref
parseInput = do
    string "input"
    spaces
    id <- InputId <$> int
    lift $ B.inputBitN id

parseConst :: Gate g => AcircParser g Ref
parseConst = do
    string "const"
    spaces
    id  <- int
    val <- (IM.! id) . view acirc_st_const_vals <$> getSt
    lift $ B.constant val

parseSecret :: Gate g => AcircParser g Ref
parseSecret = do
    string "secret"
    spaces
    id  <- int
    val <- (IM.! id) . view acirc_st_secret_vals <$> getSt
    lift $ B.secret val

parseNot :: Gate g => AcircParser g Ref
parseNot = do
    string "not"
    spaces
    x <- tr =<< parseRef
    lift $ B.circNot x

parseMul :: Gate g => AcircParser g Ref
parseMul = do
    string "mul"
    spaces
    x <- tr =<< parseRef
    spaces
    y <- tr =<< parseRef
    lift $ B.circMul x y

parseAdd :: Gate g => AcircParser g Ref
parseAdd = do
    string "add"
    spaces
    x <- tr =<< parseRef
    spaces
    y <- tr =<< parseRef
    lift $ B.circAdd x y

parseSub :: Gate g => AcircParser g Ref
parseSub = do
    string "sub"
    spaces
    x <- tr =<< parseRef
    spaces
    y <- tr =<< parseRef
    lift $ B.circSub x y

parseTimesUsed :: Ref -> AcircParser g ()
parseTimesUsed ref = optional $ do
    spaces
    char ':'
    spaces
    void int
    optional $ do
        spaces
        s <- try (string "save") <|> string "skip"
        case s of
            "save" -> lift $ B.markSave ref
            "skip" -> lift $ B.markSkip ref
