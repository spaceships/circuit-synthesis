{-# LANGUAGE OverloadedStrings #-}
module Circuit.Graphviz
  ( pprCircuit
  ) where

import Circuit
import Control.Monad.Reader

import Data.Map.Strict ((!))
import Text.PrettyPrint.Leijen.Text

pprCircuit c = renderPretty 1.0 80 $ pprCircuit' c

pprCircuit' :: Circuit -> Doc
pprCircuit' c = text "digraph circ {" <> linebreak
                 <> vsep (runReader (sequence $ fmap pprRef outs) c) <> linebreak
                 <> text "}"
  where
    outs = circ_outputs c

label :: Doc -> Doc
label t = text "[label=\"" <> t <> text "\"];"

pprInput :: Int -> Doc
pprInput i = int i <+> label (text "input" <+> int i)

-- Dot allows you to group a bunch of nodes together in braces, this
-- helps wrap that up
groupRefs :: [Ref] -> Doc
groupRefs [r] = int $ getRef r
groupRefs rs = encloseSep lbrace rbrace semi (fmap (int . getRef) rs)

-- This is the directed edge in Dot
infix 7 -->
(-->) :: Doc -> Doc -> Doc
a --> b = a <+> text "->" <+> b <> semi

pprOp :: Int -> Op -> Reader Circuit Doc
pprOp i (OpNAdd rs)  = pprOp' i rs (text "+")
pprOp i (OpSub rs)   = pprOp' i rs (text "-")
pprOp i (OpMul rs)   = pprOp' i rs (text "*")
pprOp _ (OpInput i)  = pure $ pprInput $ getId i
pprOp _ (OpSecret _) = undefined

pprOp' :: Int -> [Ref] -> Doc -> Reader Circuit Doc
pprOp' i rs op = do
  rs' <- mapM pprRef rs
  let docOp = int i <+> label op
  pure $ vsep rs' <$$>
         docOp <$$>
         groupRefs rs --> int i


pprRef :: Ref -> Reader Circuit Doc
pprRef r@(Ref i) = do
  c <- ask
  let op = circ_refmap c ! r
  pprOp i op
