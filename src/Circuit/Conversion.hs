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

instance ToCirc ArithGate  where toCirc = fromAcirc
instance ToCirc ArithGate2 where toCirc = fromAcirc . toAcirc
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

instance ToCirc2 ArithGate  where toCirc2 = fromAcirc
instance ToCirc2 ArithGate2 where toCirc2 = fromAcirc . toAcirc
instance ToCirc2 BoolGate   where toCirc2 = fromCirc
instance ToCirc2 BoolGate2  where toCirc2 = id

--------------------------------------------------------------------------------
-- other generic utils

fromCirc :: Gate g => Circ -> Circuit g
fromCirc c = B.buildCircuit $ do
    xs <- A.listArray (InputId 0,  InputId (ninputs c-1)) . concat <$> B.exportSymbols c
    ys <- A.listArray (ConstId 0,  ConstId (nconsts c-1))   <$> B.exportConsts c
    zs <- A.listArray (SecretId 0, SecretId (nsecrets c-1)) <$> B.exportSecrets c
    let eval (BoolXor _ _) _ [x,y] = B.circAdd x y
        eval (BoolAnd _ _) _ [x,y] = B.circMul x y
        eval (BoolNot _)   _ [x]   = B.circNot x
        eval (BoolBase (Input  id)) _ _ = return $ xs A.! id
        eval (BoolBase (Const  id)) _ _ = return $ ys A.! id
        eval (BoolBase (Secret id)) _ _ = return $ zs A.! id
    outs <- foldCircM eval c
    B.outputs outs

fromCirc2 :: Gate g => Circ2 -> Circuit g
fromCirc2 c = B.buildCircuit $ do
    xs <- A.listArray (InputId 0,  InputId (ninputs c-1)) . concat <$> B.exportSymbols c
    ys <- A.listArray (ConstId 0,  ConstId (nconsts c-1))   <$> B.exportConsts c
    zs <- A.listArray (SecretId 0, SecretId (nsecrets c-1)) <$> B.exportSecrets c
    let eval (Bool2Xor _ _) _ [x,y] = B.circAdd x y
        eval (Bool2And _ _) _ [x,y] = B.circMul x y
        eval (Bool2Base (Input  id)) _ _ = return $ xs A.! id
        eval (Bool2Base (Const  id)) _ _ = return $ ys A.! id
        eval (Bool2Base (Secret id)) _ _ = return $ zs A.! id
    outs <- foldCircM eval c
    B.outputs outs

fromAcirc :: Gate g => Acirc -> Circuit g
fromAcirc c = B.buildCircuit $ do
    xs <- A.listArray (InputId 0,  InputId (ninputs c-1)) . concat <$> B.exportSymbols c
    ys <- A.listArray (ConstId 0,  ConstId (nconsts c-1))   <$> B.exportConsts c
    zs <- A.listArray (SecretId 0, SecretId (nsecrets c-1)) <$> B.exportSecrets c
    let eval (ArithAdd _ _) _ [x,y] = B.circAdd x y
        eval (ArithSub _ _) _ [x,y] = B.circSub x y
        eval (ArithMul _ _) _ [x,y] = B.circMul x y
        eval (ArithBase (Input  id)) _ _ = return $ xs A.! id
        eval (ArithBase (Const  id)) _ _ = return $ ys A.! id
        eval (ArithBase (Secret id)) _ _ = return $ zs A.! id
    outs <- foldCircM eval c
    B.outputs outs

-- XXX: mangles symbols
fixInputBits :: Gate g => [(InputId, Int)] -> Circuit g -> Circuit g
fixInputBits assignments c = B.buildCircuit $ do
    ys <- B.exportConsts c
    zs <- B.exportSecrets c
    let aMap = IM.fromList (map (over _1 getInputId) assignments)
    xs <- forM (IM.toList (c^.circ_inputs)) $ \(id, ref) -> do
        case IM.lookup id aMap of
            Nothing  -> B.inputBit
            Just val -> B.secret val
    outs <- B.subcircuit' c xs ys zs
    B.outputs outs
