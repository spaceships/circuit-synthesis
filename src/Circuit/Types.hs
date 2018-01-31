{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE Strict #-}
#endif

module Circuit.Types where

import Circuit.Utils (xor, b2i, i2b)

import Lens.Micro.Platform
import Data.Array (Ix)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Vector as V

newtype Ref      = Ref      { getRef      :: Int } deriving (Eq, Ord, Num, Enum, Ix)
newtype InputId  = InputId  { getInputId  :: Int } deriving (Eq, Ord, Num, Enum, Ix)
newtype ConstId  = ConstId  { getConstId  :: Int } deriving (Eq, Ord, Num, Enum, Ix)
newtype SecretId = SecretId { getSecretId :: Int } deriving (Eq, Ord, Num, Enum, Ix)
newtype SymId    = SymId    { getSymId    :: Int } deriving (Eq, Ord, Num, Enum, Ix)

instance Show Ref      where show = show . getRef
instance Show InputId  where show = show . getInputId
instance Show ConstId  where show = show . getConstId
instance Show SecretId where show = show . getSecretId
instance Show SymId    where show = show . getSymId

data Circuit gate = Circuit
    { _circ_outputs     :: !(V.Vector Ref)
    , _circ_inputs      :: !(IM.IntMap Ref) -- Map Id Ref
    , _circ_consts      :: !(IM.IntMap Ref) -- Map Id Ref
    , _circ_secrets     :: !(IM.IntMap Ref) -- Map Id Ref
    , _circ_refmap      :: !(IM.IntMap gate)
    , _circ_const_vals  :: !(IM.IntMap Int)
    , _circ_secret_vals :: !(IM.IntMap Int)
    , _circ_symlen      :: !(IM.IntMap Int)
    , _circ_maxref      :: !Int
    , _circ_sigma_vecs  :: !(IS.IntSet) -- input symbols which should be unary sigma vectors
    , _circ_refcount    :: !(IM.IntMap Int) -- number of times each ref is used
    , _circ_refsave     :: !(IS.IntSet)
    , _circ_refskip     :: !(IS.IntSet)
    } deriving (Show)

makeLenses ''Circuit

type TestCase = ([Int], [Int])

--------------------------------------------------------------------------------
-- types of gates

data BaseGate = Input  !InputId
              | Const  !ConstId
              | Secret !SecretId
              deriving (Eq, Ord, Show)

data ArithGate =
      ArithAdd !Ref !Ref
    | ArithSub !Ref !Ref
    | ArithMul !Ref !Ref
    | ArithBase !BaseGate
    deriving (Eq, Ord, Show)

-- Acirc2 has free Xor and mod 2
newtype ArithGate2 = ArithGate2 { getArithGate :: ArithGate } deriving (Eq, Ord, Show)

data BoolGate =
      BoolXor !Ref !Ref
    | BoolAnd !Ref !Ref
    | BoolNot !Ref
    | BoolBase !BaseGate
    deriving (Eq, Ord, Show)

-- Bool2 has no Not gates
data BoolGate2 =
       Bool2Xor !Ref !Ref
     | Bool2And !Ref !Ref
     | Bool2Base !BaseGate
     deriving (Eq, Ord, Show)

type Acirc = Circuit ArithGate
type Acirc2 = Circuit ArithGate2
type Circ = Circuit BoolGate
type Circ2 = Circuit BoolGate2

---------------------------------------------------------------------------------------
-- Gate class allows us to share boilerplate between binary and arithmetic circuits

class (Eq g, Ord g) => Gate g where
    gateArgs :: g -> [Ref]
    gateGetBase :: g -> Maybe BaseGate
    gateEval :: (BaseGate -> Int) -> g -> [Int] -> Int

    gateAdd :: Ref -> Ref -> g
    gateSub :: Ref -> Ref -> g
    gateMul :: Ref -> Ref -> g
    gateXor :: Ref -> Ref -> Maybe g
    gateNot :: Ref -> Maybe g
    gateBase :: BaseGate -> g

    gateIsMul :: g -> Bool
    gateIsGate :: g -> Bool

    gateFix :: g -> [Ref] -> g

instance Gate ArithGate where
    gateArgs (ArithAdd x y)  = [x,y]
    gateArgs (ArithSub x y)  = [x,y]
    gateArgs (ArithMul x y)  = [x,y]
    gateArgs (ArithBase _) = []

    gateGetBase (ArithBase b) = Just b
    gateGetBase _ = Nothing

    gateEval _ (ArithAdd _ _) [x,y] = x + y
    gateEval _ (ArithSub _ _) [x,y] = x - y
    gateEval _ (ArithMul _ _) [x,y] = x * y
    gateEval getBase (ArithBase b) [] = getBase b

    gateAdd x y = ArithAdd x y
    gateSub x y = ArithSub x y
    gateMul x y = ArithMul x y
    gateXor _ _ = Nothing
    gateNot _   = Nothing
    gateBase = ArithBase

    gateIsMul (ArithMul _ _) = True
    gateIsMul _ = False

    gateIsGate (ArithBase _) = False
    gateIsGate _ = True

    gateFix (ArithAdd _ _) [x,y] = ArithAdd x y
    gateFix (ArithSub _ _) [x,y] = ArithSub x y
    gateFix (ArithMul _ _) [x,y] = ArithMul x y

instance Gate ArithGate2 where
    gateArgs = gateArgs . getArithGate
    gateGetBase = gateGetBase . getArithGate
    gateEval b g a = gateEval b (getArithGate g) a `mod` 2
    gateAdd x y = ArithGate2 (gateAdd x y)
    gateSub x y = ArithGate2 (gateSub x y)
    gateMul x y = ArithGate2 (gateMul x y)
    gateXor x y = Just $ ArithGate2 (gateAdd x y)
    gateNot _   = Nothing
    gateBase b = ArithGate2 (ArithBase b)
    gateIsMul = gateIsMul . getArithGate
    gateIsGate = gateIsGate . getArithGate
    gateFix g args = ArithGate2 (gateFix (getArithGate g) args)

instance Gate BoolGate where
    gateArgs (BoolXor x y) = [x,y]
    gateArgs (BoolAnd x y) = [x,y]
    gateArgs (BoolNot x)   = [x]
    gateArgs (BoolBase _)  = []

    gateGetBase (BoolBase b) = Just b
    gateGetBase _ = Nothing

    gateEval _ (BoolXor _ _) [x,y] = b2i (i2b x `xor` i2b y)
    gateEval _ (BoolAnd _ _) [x,y] = b2i (i2b x && i2b y)
    gateEval _ (BoolNot _)   [x]   = b2i (not (i2b x))
    gateEval getBase (BoolBase b) [] = getBase b

    gateAdd x y = BoolXor x y
    gateSub x y = BoolXor x y
    gateMul x y = BoolAnd x y
    gateXor x y = Just (BoolXor x y)
    gateNot x   = Just (BoolNot x)
    gateBase b  = BoolBase b

    gateIsMul (BoolAnd _ _) = True
    gateIsMul _ = False

    gateIsGate (BoolBase _) = False
    gateIsGate _ = True

    gateFix (BoolXor _ _) [x,y] = BoolXor x y
    gateFix (BoolAnd _ _) [x,y] = BoolAnd x y
    gateFix (BoolNot _) [x] = BoolNot x

instance Gate BoolGate2 where
    gateArgs (Bool2Xor x y) = [x,y]
    gateArgs (Bool2And x y) = [x,y]
    gateArgs (Bool2Base _) = []

    gateGetBase (Bool2Base b) = Just b
    gateGetBase _ = Nothing

    gateEval _ (Bool2Xor _ _) [x,y] = b2i (i2b x `xor` i2b y)
    gateEval _ (Bool2And _ _) [x,y] = b2i (i2b x && i2b y)
    gateEval getBase (Bool2Base b) [] = getBase b

    gateAdd x y = Bool2Xor x y
    gateSub x y = Bool2Xor x y
    gateMul x y = Bool2And x y
    gateXor x y = Just (Bool2Xor x y)
    gateNot _   = Nothing
    gateBase = Bool2Base

    gateIsMul (Bool2And _ _) = True
    gateIsMul _ = False

    gateIsGate (Bool2Base _) = False
    gateIsGate _ = True

    gateFix (Bool2Xor _ _) [x,y] = Bool2Xor x y
    gateFix (Bool2And _ _) [x,y] = Bool2And x y
