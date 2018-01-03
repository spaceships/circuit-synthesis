{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE Strict #-}
#endif

module Circuit.Types where

import Circuit.Utils (xor, b2i, i2b)

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Lens.Micro.Platform

newtype Ref = Ref { getRef :: Int } deriving (Eq, Ord, Num)
newtype Id  = Id  { getId  :: Int } deriving (Eq, Ord, Num)

instance Show Ref where show ref = show (getRef ref)
instance Show Id where show id = show (getId id)

data Circuit gate = Circuit
    { _circ_outputs     :: !IS.IntSet
    , _circ_inputs      :: !IS.IntSet
    , _circ_consts      :: !(IM.IntMap Id)
    , _circ_secret_refs :: !(IS.IntSet)
    , _circ_secret_ids  :: !(IS.IntSet)
    , _circ_refmap      :: !(IM.IntMap gate)
    , _circ_const_vals  :: !(IM.IntMap Integer)
    , _circ_symlen      :: !Int
    , _circ_base        :: !Integer
    , _circ_refcount    :: !(IM.IntMap Int)
    } deriving (Show)

makeLenses ''Circuit

type TestCase = ([Integer], [Integer])

--------------------------------------------------------------------------------
-- types of gates

data ArithGate =
      ArithAdd !Ref !Ref
    | ArithSub !Ref !Ref
    | ArithMul !Ref !Ref
    | ArithInput !Id
    | ArithConst !Id
    deriving (Eq, Ord, Show)

-- Acirc2 has free Xor and mod 2
newtype ArithGate2 = ArithGate2 { getArithGate :: ArithGate } deriving (Eq, Ord, Show)

data BoolGate =
      BoolXor !Ref !Ref
    | BoolAnd !Ref !Ref
    | BoolNot !Ref
    | BoolInput !Id
    | BoolConst !Id
    deriving (Eq, Ord, Show)

-- Bool2 has no Not gates- they're folded into the Xor and And gates with negation flags
data BoolGate2 =
       Bool2Xor !Ref Bool !Ref Bool
     | Bool2And !Ref Bool !Ref Bool
     | Bool2Input !Id
     | Bool2Const !Id
     deriving (Eq, Ord, Show)

type Acirc = Circuit ArithGate
type Acirc2 = Circuit ArithGate2
type Circ = Circuit BoolGate
type Circ2 = Circuit BoolGate2

---------------------------------------------------------------------------------------
-- Gate class allows us to share boilerplate between binary and arithmetic circuits

class (Eq g, Ord g) => Gate g where
    gateArgs :: g -> [Ref]
    gateEval :: (Id -> Integer) -> (Id -> Integer) -> g -> [Integer] -> Integer
    gateAdd :: Ref -> Ref -> g
    gateSub :: Ref -> Ref -> g
    gateMul :: Ref -> Ref -> g
    gateXor :: Ref -> Ref -> Maybe g
    gateNot :: Ref -> Maybe g
    gateInput :: Id -> g
    gateConst :: Id -> g
    gateIsMul :: g -> Bool
    gateIsGate :: g -> Bool

instance Gate ArithGate where
    gateArgs (ArithAdd x y)  = [x,y]
    gateArgs (ArithSub x y)  = [x,y]
    gateArgs (ArithMul x y)  = [x,y]
    gateArgs (ArithInput _) = []
    gateArgs (ArithConst _) = []

    gateEval _ _ (ArithAdd _ _) [x,y] = x + y
    gateEval _ _ (ArithSub _ _) [x,y] = x - y
    gateEval _ _ (ArithMul _ _) [x,y] = x * y
    gateEval getInp _   (ArithInput i) [] = getInp i
    gateEval _ getConst (ArithConst i) [] = getConst i

    gateAdd x y = ArithAdd x y
    gateSub x y = ArithSub x y
    gateMul x y = ArithMul x y
    gateXor _ _ = Nothing
    gateNot _   = Nothing
    gateInput i = ArithInput i
    gateConst i = ArithConst i

    gateIsMul (ArithMul _ _) = True
    gateIsMul _ = False

    gateIsGate (ArithInput _) = False
    gateIsGate (ArithConst _) = False
    gateIsGate _ = True

instance Gate ArithGate2 where
    gateArgs = gateArgs . getArithGate
    gateEval i c g a = gateEval i c (getArithGate g) a `mod` 2
    gateAdd x y = ArithGate2 (gateAdd x y)
    gateSub x y = ArithGate2 (gateSub x y)
    gateMul x y = ArithGate2 (gateMul x y)
    gateXor x y = Just $ ArithGate2 (gateAdd x y)
    gateNot _   = Nothing
    gateInput i = ArithGate2 (ArithInput i)
    gateConst i = ArithGate2 (ArithConst i)

    gateIsMul = gateIsMul . getArithGate
    gateIsGate = gateIsGate . getArithGate

instance Gate BoolGate where
    gateArgs (BoolXor x y) = [x,y]
    gateArgs (BoolAnd x y) = [x,y]
    gateArgs (BoolNot x)   = [x]
    gateArgs (BoolInput _) = []
    gateArgs (BoolConst _) = []

    gateEval _ _ (BoolXor _ _) [x,y] = b2i (i2b x `xor` i2b y)
    gateEval _ _ (BoolAnd _ _) [x,y] = x * y
    gateEval _ _ (BoolNot _)   [x]   = 1 - x
    gateEval getInp _   (BoolInput i) [] = getInp i
    gateEval _ getConst (BoolConst i) [] = getConst i

    gateAdd x y = BoolXor x y
    gateSub x y = BoolXor x y
    gateMul x y = BoolAnd x y
    gateXor x y = Just (BoolXor x y)
    gateNot x   = Just (BoolNot x)
    gateInput i = BoolInput i
    gateConst i = BoolConst i

    gateIsMul (BoolAnd _ _) = True
    gateIsMul _ = False

    gateIsGate (BoolInput _) = False
    gateIsGate (BoolConst _) = False
    gateIsGate _ = True

instance Gate BoolGate2 where
    gateArgs (Bool2Xor x _ y _) = [x,y]
    gateArgs (Bool2And x _ y _) = [x,y]
    gateArgs (Bool2Input _) = []
    gateArgs (Bool2Const _) = []

    gateEval _ _ (Bool2Xor _ negx _ negy) [x,y] = b2i ((i2b x `xor` negx) `xor` (i2b y `xor` negy))
    gateEval _ _ (Bool2And _ negx _ negy) [x,y] = b2i ((i2b x `xor` negx) && (i2b y `xor` negy))
    gateEval getInp _   (Bool2Input i) [] = getInp i
    gateEval _ getConst (Bool2Const i) [] = getConst i

    gateAdd x y = Bool2Xor x False y False
    gateSub x y = Bool2Xor x False y False
    gateMul x y = Bool2And x False y False
    gateXor x y = Just (Bool2Xor x False y False)
    gateNot _   = Nothing
    gateInput i = Bool2Input i
    gateConst i = Bool2Const i

    gateIsMul (Bool2And _ _ _ _) = True
    gateIsMul _ = False

    gateIsGate (Bool2Input _) = False
    gateIsGate (Bool2Const _) = False
    gateIsGate _ = True
