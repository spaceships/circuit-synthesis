{-# LANGUAGE OverloadedStrings #-}
module Circuit.Format.Graphviz (Graphviz(..)) where

import Circuit
import Circuit.Conversion

import Control.Monad.Reader
import Control.Monad.State

import Data.List (sort, group, partition)
import qualified Data.Set as S
import Data.IntMap.Strict ((!))

import Text.PrettyPrint.Leijen.Text hiding (group)
import Data.Text.Lazy (Text, toStrict)
import qualified Data.Text as T

class Graphviz g where
    showGraphviz :: Circuit g -> T.Text
    showGraphviz = error "graphviz undefined for this circuit type"
instance Graphviz BoolGate where
instance Graphviz ArithGate where
    showGraphviz = showCircuitT
instance Graphviz ArithGate2 where
    showGraphviz = showCircuitT . toAcirc

-- Toplevel rendering of a circuit with 80 characters width default
showCircuitT :: Circuit ArithGate -> T.Text
showCircuitT = toStrict . pprCircuit 80

-- Toplevel rendering of a circuit
pprCircuit :: Int -> Circuit ArithGate -> Text
pprCircuit width c = displayT $ renderPretty 1.0 width $ pprCircuit' c

-- Same as showCircuitT but resulting in a String
showCircuit :: Circuit ArithGate -> String
showCircuit = T.unpack . showCircuitT

-- Pretty print a circuit by following its edges from the set of outputs.
pprCircuit' :: Circuit ArithGate -> Doc
pprCircuit' c = text "digraph circ {" <> linebreak
                 <> indent 4 douts    <> linebreak
                 <> text "}"          <> linebreak
  where
    outs  = outputRefs c
    douts = vsep (runPrettyPr c S.empty (sequence $ fmap pprRef outs))


-- Monad for state and environment during pretty printing
-- We need the state so that we know what we've already rendered
type PrettyPr a = StateT (S.Set Int) (Reader (Circuit ArithGate)) a

-- Run a pretty printing
-- Output is a list of `Doc`s because it's one for each output
runPrettyPr :: Circuit ArithGate -> S.Set Int -> PrettyPr [Doc] -> [Doc]
runPrettyPr env st = fst . (flip runReader) env . (flip runStateT) st

-- Helper function for determining whether something is in the already-rendered
-- set
isRendered :: Int -> PrettyPr Bool
isRendered i = state $ \s -> (i `S.member` s, S.insert i s)

label :: Doc -> Doc
label t = text "[label=\"" <> t <> text "\"];"

pprInput :: Int -> InputId -> Doc
pprInput i id = int i <+> label (text "input" <+> int (getInputId id))

pprConst :: Int -> ConstId -> Doc
pprConst i id = int i <+> label (text "const" <+> int (getConstId id))

pprSecret :: Int -> SecretId -> Doc
pprSecret i id = int i <+> label (text "secret" <+> int (getSecretId id))

mkGroup :: [Doc] -> Doc
mkGroup [d] = d
mkGroup ds  = encloseSep lbrace rbrace semi ds

mkRepeats :: [[Int]] -> Int -> Doc
mkRepeats ss t = vsep $ fmap mkEdge ss'
  where
    ss'      = concat $ fmap (drop 1) ss
    mkEdge i = int i --> int t

-- Dot allows you to group a bunch of nodes together in braces, this
-- helps wrap that up.
-- Unfortunately in DOT syntax
-- { a; a } -> b
-- does not mean the same thing as
-- a -> b
-- a -> b
-- Which means that we have to account for that :'(
groupRefs :: [Ref] -> Int -> Doc
groupRefs [r] i = int (getRef r) --> int i
groupRefs rs  i = grp <$$> mkRepeats dups i
  where
    grp           = mkGroup (fmap int uniqs) --> int i
    (dups, sings) = partition ((> 1) . length) $ group $ sort $ fmap getRef rs
    uniqs         = fmap head dups ++ concat sings
                    -- `head` is safe because we know the
                    -- elements in `dups` have (length > 1)


-- This is the directed edge in Dot
infix 7 -->
(-->) :: Doc -> Doc -> Doc
a --> b = a <+> text "->" <+> b <> semi

pprOp :: Int -> ArithGate -> PrettyPr Doc
pprOp i (ArithAdd x y)  = pprOp' i [x,y] (text "+")
pprOp i (ArithSub x y)  = pprOp' i [x,y] (text "-")
pprOp i (ArithMul x y)  = pprOp' i [x,y] (text "*")
pprOp i (ArithBase (Input  id)) = pure $ pprInput  i id
pprOp i (ArithBase (Secret id)) = pure $ pprSecret i id
pprOp i (ArithBase (Const  id)) = pure $ pprConst i id

pprOp' :: Int -> [Ref] -> Doc -> PrettyPr Doc
pprOp' i rs op = do
  rs' <- mapM pprRef rs
  let docOp = int i <+> label op
  pure $ vsep rs' <$$>
         docOp <$$>
         groupRefs rs i


pprRef :: Ref -> PrettyPr Doc
pprRef r@(Ref i) = do
  b <- isRendered i
  case b of
    False -> do
      c <- ask
      let op = _circ_refmap c ! getRef r
      pprOp i op
    True -> return empty
