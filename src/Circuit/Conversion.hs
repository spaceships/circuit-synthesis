module Circuit.Conversion where

import Circuit
import qualified Circuit.Builder as B

import Control.Monad.State.Strict
import Lens.Micro.Platform
import qualified Data.IntMap as IM
import qualified Data.Array as A

--------------------------------------------------------------------------------

class ToAcirc g where
    toAcirc :: Circuit g -> Acirc
    toAcirc = error "no conversion to Acirc defined"

instance ToAcirc ArithGate  where toAcirc = id
instance ToAcirc ArithGate2 where toAcirc = circ_refmap . each %~ getArithGate
instance ToAcirc BoolGate   where toAcirc = fromCirc
instance ToAcirc BoolGate2  where toAcirc = fromCirc2

--------------------------------------------------------------------------------

class ToCirc g where
    toCirc :: Circuit g -> Circ
    toCirc = error "no conversion to binary circuit defined"

instance ToCirc ArithGate  where
instance ToCirc ArithGate2 where
instance ToCirc BoolGate   where toCirc = id
instance ToCirc BoolGate2  where toCirc = fromCirc2

--------------------------------------------------------------------------------

class ToAcirc2 g where
    toAcirc2 :: Circuit g -> Acirc2
    toAcirc2 = error "no conversion to Acirc2 defined"

instance ToAcirc2 ArithGate  where toAcirc2 = circ_refmap . each %~ ArithGate2
instance ToAcirc2 ArithGate2 where toAcirc2 = id
instance ToAcirc2 BoolGate   where toAcirc2 = fromCirc
instance ToAcirc2 BoolGate2  where toAcirc2 = fromCirc2

--------------------------------------------------------------------------------

class ToCirc2 g where
    toCirc2 :: Circuit g -> Circ2
    toCirc2 = error "no conversion to Circ2 defined"

instance ToCirc2 ArithGate  where
instance ToCirc2 ArithGate2 where
instance ToCirc2 BoolGate   where toCirc2 = fromCirc
instance ToCirc2 BoolGate2  where toCirc2 = id

--------------------------------------------------------------------------------
-- other generic utils

fixInputBits :: Gate g => [(Id, Int)] -> Circuit g -> Circuit g
fixInputBits assignments c = B.buildCircuit $ do
    B.exportParams c
    ys <- B.exportConsts c
    let aMap = IM.fromList (map (over _1 getId) assignments)
    xs <- forM (IM.toList (c^.circ_inputs)) $ \(id, ref) -> do
        case IM.lookup id aMap of
            Nothing  -> B.input
            Just val -> B.secret val
    zs <- B.subcircuit' c xs ys
    B.outputs zs

fromCirc :: Gate g => Circ -> Circuit g
fromCirc c = B.buildCircuit $ do
    B.exportParams c
    xs <- A.listArray (0,ninputs c-1) <$> B.inputs (ninputs c)
    ys <- A.listArray (0,nconsts c-1) <$> B.exportConsts c
    let eval (BoolXor _ _) _ [x,y] = B.circAdd x y
        eval (BoolAnd _ _) _ [x,y] = B.circMul x y
        eval (BoolNot _)   _ [x]   = B.circNot x
        eval (BoolBase (Input id)) _ _ = return $ xs A.! getId id
        eval (BoolBase (Const id)) _ _ = return $ ys A.! getId id
    outs <- foldCircM eval c
    B.outputs outs

fromCirc2 :: Gate g => Circ2 -> Circuit g
fromCirc2 c = B.buildCircuit $ do
    B.exportParams c
    xs <- A.listArray (0,ninputs c-1) <$> B.inputs (ninputs c)
    ys <- A.listArray (0,nconsts c-1) <$> B.exportConsts c
    let eval (Bool2Xor _ _) _ [x,y] = B.circAdd x y
        eval (Bool2And _ _) _ [x,y] = B.circMul x y
        eval (Bool2Base (Input id)) _ _ = return $ xs A.! getId id
        eval (Bool2Base (Const id)) _ _ = return $ ys A.! getId id
    outs <- foldCircM eval c
    B.outputs outs

