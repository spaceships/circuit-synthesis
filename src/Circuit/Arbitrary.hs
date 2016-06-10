module Zim15.Circuit.Arbitrary
  ( arbitraryCircuit
  )
where

import Zim15.Circuit
import Zim15.Util (b2i)

import Control.Monad.State.Strict
import Test.QuickCheck hiding (verbose)
import Test.QuickCheck.Gen
import Data.Map ((!))
import qualified Data.Map as M

--------------------------------------------------------------------------------
-- this could be an arbitrary instance, but I just need random circuits for now

data CircuitBuilder = CircuitBuilder {
    circ     :: Circuit,
    levels   :: M.Map Int [Ref],
    curRef   :: Ref,
    curInput :: Ref,
    curConst :: Ref
}

arbitraryCircuit :: Int -> Int -> Int -> Int -> Gen Circuit
arbitraryCircuit nxs nys width circdepth = circ <$> build
  where
    build :: Gen CircuitBuilder
    build = flip execStateT emptyBuild $ do
        replicateM nxs    makeInp
        replicateM nys    makeConst
        forM [1..circdepth] $ \d -> do
            ngates <- lift $ choose (1, width)
            replicateM_ ngates (makeGate d)
        lastRef <- makeGate (circdepth + 1)
        setOutRef lastRef

    emptyBuild = CircuitBuilder emptyCirc M.empty 0 0 0
    emptyCirc  = Circuit 0 M.empty M.empty []

makeInp :: StateT CircuitBuilder Gen ()
makeInp = do
    i   <- nextInput
    ref <- putGate 0 (Input i)
    putInput i ref

makeConst :: StateT CircuitBuilder Gen ()
makeConst = do
    i   <- nextConst
    val <- lift $ arbitrary
    putConst (b2i val)
    putGate 0 (Const i)
    return ()

-- make a gate using refs from the level immediately below it, with high
-- probability (high probability, so we get a few "stale" refs)
makeGate :: Int -> StateT CircuitBuilder Gen Ref
makeGate level = loop 100
  where
    loop :: Int -> StateT CircuitBuilder Gen Ref
    loop 0 = error "[makeGate] ran out of tries"
    loop i = do
        typ   <- lift $ choose (0,2)
        stale <- lift $ frequency [(10, falseGen), (1, trueGen)]
        xref  <- existingRef (level-1) stale
        yref  <- existingRef (level-1) stale
        if (xref == yref) then
            loop (i+1)
        else case (typ :: Int) of
            0 -> putGate level (Add xref yref)
            1 -> putGate level (Sub xref yref)
            2 -> putGate level (Mul xref yref)
            _ -> error "oops"

putGate :: Int -> Op -> StateT CircuitBuilder Gen Ref
putGate level op = do
    m   <- gets (refMap . circ)
    ref <- nextRef
    let m' = M.insert ref op m
    modify $ \s -> s { circ   = (circ s) { refMap = m'}
                     , levels = M.insertWith (++) level [ref] (levels s)
                     }
    return ref

existingRef :: Int -> Bool -> StateT CircuitBuilder Gen Ref
existingRef _     True  = (lift . elements . M.keys) =<< gets (refMap . circ)
existingRef level False = (lift . elements)          =<< gets ((! level) . levels)

nextRef :: StateT CircuitBuilder Gen Ref
nextRef = do
    ref <- gets curRef
    modify $ \s -> s { curRef = ref + 1 }
    return ref

nextInput :: StateT CircuitBuilder Gen ID
nextInput = do
    i <- gets curInput
    modify $ \s -> s { curInput = i + 1 }
    return i

nextConst :: StateT CircuitBuilder Gen ID
nextConst = do
    i <- gets curConst
    modify $ \s -> s { curConst = i + 1 }
    return i

putConst :: Integer -> StateT CircuitBuilder Gen ()
putConst val = modify $ \s -> s { circ = (circ s) { consts = val : (consts (circ s)) } }

putInput :: ID -> Ref -> StateT CircuitBuilder Gen ()
putInput i ref = modify $ \s -> s { circ = (circ s) { inpRefs = M.insert i ref (inpRefs (circ s)) } }

setOutRef :: Ref -> StateT CircuitBuilder Gen ()
setOutRef ref = modify $ \s -> s { circ = (circ s) { outRef = ref } }

falseGen :: Gen Bool
falseGen = MkGen (\_ _ -> False)

trueGen :: Gen Bool
trueGen = MkGen (\_ _ -> True)
