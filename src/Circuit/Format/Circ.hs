{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Circuit.Format.Circ where

import Circuit
import Circuit.Conversion
import Circuit.Utils

import Control.Monad
import Data.Maybe
import Formatting ((%))
import Lens.Micro.Platform
import TextShow
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.IntMap as IM
import qualified Formatting as F

-- read :: FilePath -> IO Circ
-- read = fmap fst . readWithTests

-- readWithTests :: FilePath -> IO (Acirc, [TestCase])
-- readWithTests fp = parse <$> readFile fp

write :: ToCirc g => FilePath -> Circuit g -> IO ()
write fp c = T.writeFile fp (showWithTests c [])

writeWithTests :: ToCirc g => FilePath -> Circuit g -> IO ()
writeWithTests fp c' = do
    let c = toCirc c'
    ts <- replicateM 10 (genTest c)
    T.writeFile fp (showWithTests c ts)

showWithTests :: ToCirc g => Circuit g -> [TestCase] -> T.Text
showWithTests !c' !ts = T.unlines (header ++ inputs ++ consts ++ gates)
  where
    c = toCirc c'

    header = [ T.append ":ninputs " (showt (ninputs c))
             , T.append ":consts "  (T.unwords (map showt (IM.elems (_circ_const_vals c))))
             , T.append ":nrefs "   (showt (c ^. circ_maxref))
             , T.append ":outputs " (T.unwords (map (showt.getRef) (outputRefs c)))
             , T.append ":secrets " (T.unwords (map (showt.getRef) (secretRefs c)))
             ] ++ map showTest ts
               ++ [ ":start" ]

    inputs = mapMaybe gateTxt (inputRefs c)
    consts = mapMaybe gateTxt (constRefs c)
    gates  = mapMaybe gateTxt (gateRefs c)

    gateTxt :: Ref -> Maybe T.Text
    gateTxt !ref =
        case c ^. circ_refcount . at (getRef ref) of
            Nothing -> Nothing
            Just ct -> Just $ case c ^. circ_refmap . at (getRef ref) . non (error "[gateTxt] unknown ref") of
                (BoolBase (Input id)) -> F.sformat (F.shown % " input " % F.shown) ref id
                (BoolBase (Const id)) -> F.sformat (F.shown % " const " % F.shown) ref id
                (BoolXor x y) -> F.sformat (F.shown % " XOR " % F.shown % " " % F.shown) ref x y
                (BoolAnd x y) -> F.sformat (F.shown % " AND " % F.shown % " " % F.shown) ref x y
                (BoolNot x)   -> F.sformat (F.shown % " NOT " % F.shown) ref x

showTest :: TestCase -> T.Text
showTest (!inp, !out) = T.concat [":test ", T.pack (showInts inp), " ", T.pack (showInts out) ]
