module Circuit.Conversion where

import Circuit
import qualified Circuit.Builder as B
import qualified Circuit.Builder.Internals as B

import Control.Monad.State.Strict
import Lens.Micro.Platform
import qualified Data.IntMap as IM

--------------------------------------------------------------------------------

class ToAcirc g where
    toAcirc :: Circuit g -> Acirc
    toAcirc = error "no conversion to Acirc defined"

instance ToAcirc BoolGate where
    -- TODO: define me

instance ToAcirc ArithGate where
    toAcirc = id

instance ToAcirc ArithGate2 where
    toAcirc = circ_refmap . each %~ getArithGate

--------------------------------------------------------------------------------

class ToCirc g where
    toCirc :: Circuit g -> Circ
    toCirc = error "no conversion to binary circuit defined"

instance ToCirc ArithGate where

instance ToCirc ArithGate2 where

instance ToCirc BoolGate where
    toCirc = id

--------------------------------------------------------------------------------

class ToAcirc2 g where
    toAcirc2 :: Circuit g -> Acirc2
    toAcirc2 = error "no conversion to Acirc2 defined"

instance ToAcirc2 ArithGate2 where
    toAcirc2 = id

instance ToAcirc2 ArithGate where
    toAcirc2 = circ_refmap . each %~ ArithGate2

instance ToAcirc2 BoolGate where
    -- TODO: define me

--------------------------------------------------------------------------------

class ToCirc2 g where
    toCirc2 :: Circuit g -> Circ2
    toCirc2 = error "no conversion to Circ2 defined"

instance ToCirc2 ArithGate where
instance ToCirc2 ArithGate2 where

instance ToCirc2 BoolGate where
    toCirc2 = foldNots

foldNots :: Circ -> Circ2
foldNots c = flip evalState IM.empty $ B.buildCircuitT $ do
    B.exportParams c
    xs <- B.inputs (ninputs c)
    ys <- B.exportConsts c
    zipWithM update (inputRefs c) xs
    zipWithM update (constRefs c) ys
    ns <- foldCircM eval c
    when (not (all (== False) ns)) (error "[foldNots] top level negations unsupported!")
    B.outputs =<< mapM tr (outputRefs c)
  where
    update :: Ref -> Ref -> B.BuilderT g (State (IM.IntMap Ref)) ()
    update ref newRef = lift $ at (getRef ref) ?= newRef

    tr :: Ref -> B.BuilderT g (State (IM.IntMap Ref)) Ref
    tr ref = lift $ use $ at (getRef ref) . non (error ("[tr] unknown ref " ++ show ref))

    eval (BoolXor x y) z [nx, ny] = do
        z' <- B.newGate =<< Bool2Xor <$> tr x <*> pure nx <*> tr y <*> pure ny
        update z z'
        return False

    eval (BoolAnd x y) z [nx, ny] = do
        z' <- B.newGate =<< Bool2And <$> tr x <*> pure nx <*> tr y <*> pure ny
        update z z'
        return False

    eval (BoolNot x) z [n] = do
        update z =<< tr x
        return (not n)

    eval (BoolInput _) _ _ = return False
    eval (BoolConst _) _ _ = return False
