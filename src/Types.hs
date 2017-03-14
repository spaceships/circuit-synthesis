{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
module Types where

import Control.Applicative (liftA2)

-- | Top-level types in a circuit
--   Circuit references can refer to single `wires`, and bundles of wires
--   that either represent a vector or a matrix. It would be possible to
--   generalize this by have `Type` be recursive (i.e. `Vector Type`,
--   and no `Matrix` constructor), but for now we'll go with the simple
--   case
data Type' a = Wire a
             | Vector a
             | Matrix a
  deriving (Show, Eq, Functor)

type Type = Type' BaseType

type MType = Maybe Type

-- | Enumerate the possible base types
data BaseType = Integer
              | Rational
  deriving (Show, Eq)

-- | Class defining things that you can compute the
--   least upper-bound for
--   should be commutative and associative
class Lub a where
  (/\) :: a -> a -> a

infixl 5 /\

instance Lub BaseType where
  Integer /\ t2      = t2
  t1      /\ _       = t1

-- | Least upper-bound of `Type`s
instance Lub (Type' BaseType) where
  Wire bt   /\ t2        = fmap (/\ bt) t2
  t1        /\ Wire bt   = fmap (/\ bt) t1
  Matrix bt /\ t2        = Matrix (bt /\ getBT t2)
  t1        /\ Matrix bt = Matrix (bt /\ getBT t1)
  t1        /\ t2        = fmap (/\ getBT t2) t1 -- Vectors just need to lub
                                                 -- their base types
getBT :: Type -> BaseType
getBT (Wire b)   = b
getBT (Vector b) = b
getBT (Matrix b) = b


-- | Enumeration of the primitive types operators
data CircOp = CPlus
            | CMult
            | CSubt
  deriving (Show, Eq)

-- | We'll have to look up the types of operators and of references, so this
-- is just a wrapper for that fact
type Symbol = Either CircOp Int

-- | Convenience constructor for operator `Symbol`s
op :: CircOp -> Symbol
op = Left

-- | Convenience constructor for reference `Symbol`s
ref :: Int -> Symbol
ref = Right

-- | Given the types of arguments applied to an operator
--   `typeOfOp` determines the output type
mgt :: [Type] -> Type
mgt = foldr1 (/\)


-- | Often we'll be using `MType`s instead of `Type`s so we need to lift
--   operations to work on them
mgtM :: [MType] -> MType
mgtM = foldr1 (liftA2 (/\))
