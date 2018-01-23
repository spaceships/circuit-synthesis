{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Circuit.Optimizer (Optimize(..)) where

import Circuit
import Circuit.Conversion
import qualified Circuit.Builder as B

import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer
import Data.Maybe (isJust, catMaybes, listToMaybe)
import Debug.Trace
import Lens.Micro.Platform
import System.Process
import Text.Printf
import qualified Circuit.Format.Sexp as Sexp
import qualified Data.Map as M
import qualified Data.Vector as V

class Optimize g where
    optimizeO1 :: Circuit g -> IO (Circuit g)
    optimizeO2 :: Circuit g -> IO (Circuit g)
    optimizeO3 :: Circuit g -> IO (Circuit g)

    optimizeO1 = error "-O1 undefined for this type type"
    optimizeO2 = error "-O2 undefined for this type type"
    optimizeO3 = error "-O3 undefined for this type type"

instance Optimize BoolGate where
instance Optimize ArithGate where
    optimizeO1 = return . foldConsts
    optimizeO2 = flattenRec . foldConsts
    optimizeO3 = flatten . foldConsts
instance Optimize ArithGate2 where
    optimizeO1 = fmap toAcirc2 . optimizeO1 . toAcirc
    optimizeO2 = fmap toAcirc2 . optimizeO1 . toAcirc
    optimizeO3 = fmap toAcirc2 . optimizeO1 . toAcirc

circToSage :: Circuit ArithGate -> [String]
circToSage c = foldCirc eval c
  where
    eval (ArithAdd _ _) [x,y] = printf "(%s) + (%s)" x y
    eval (ArithSub _ _) [x,y] = printf "(%s) - (%s)" x y
    eval (ArithMul _ _) [x,y] = printf "(%s) * (%s)" x y
    eval (ArithBase (Input  i)) [] = printf "var('x%d')" (getInputId i)
    eval (ArithBase (Secret i)) [] = printf "var('y%d')" (getSecretId i)
    eval (ArithBase (Const  i)) [] = show (getConst c i)
    eval op args  = error ("[circToSexp] weird input: " ++ show op ++ " " ++ show args)

callSage :: Int -> String -> IO (Circuit ArithGate)
callSage ninputs s = do
    r <- readProcess "./scripts/poly-sage.sage" [] s
    -- printf "[callSage] output=\"%s\"\n" r
    return $ Sexp.parse ninputs r

flatten :: Circuit ArithGate -> IO (Circuit ArithGate)
flatten c = merge <$> mapM (callSage (ninputs c)) (circToSage c)

-- merge circuits to use the same inputs, consts, and intermediate gates
merge :: [Circuit ArithGate] -> Circuit ArithGate
merge cs = B.buildCircuit $ do
    xs   <- B.inputs (ninputs (head cs))
    outs <- concat <$> mapM (flip B.subcircuit xs) cs
    B.outputs outs

-- find the highest degree subcircuit within a given depth
find :: Int -> Circuit ArithGate -> Ref
find maxDepth c = snd $ execState (foldCircM eval c) (0, Ref 0)
  where
    eval (ArithAdd _ _) ref [(xdegs, xdepth), (ydegs, ydepth)] = do
        let degs  = M.unionWith max xdegs ydegs
            depth = max xdepth ydepth + 1
        check ref degs depth
        return (degs, depth)

    eval (ArithSub _ _) ref [(xdegs, xdepth), (ydegs, ydepth)] = do
        let degs  = M.unionWith (max) xdegs ydegs
            depth = max xdepth ydepth + 1
        check ref degs depth
        return (degs, depth)

    eval (ArithMul _ _) ref [(xdegs, xdepth), (ydegs, ydepth)] = do
        let degs  = M.unionWith (+) xdegs ydegs
            depth = max xdepth ydepth + 1
        check ref degs depth
        return (degs, depth)

    eval op@(ArithBase (Input _))   _ _ = return (M.singleton op 1, 0)
    eval op@(ArithBase (Secret id)) _ _ = return (M.singleton op 1, 0)
    eval op@(ArithBase (Const  id)) _ _ = return (M.empty         , 0)

    eval _ _ _ = undefined

    check :: Monad m => Ref -> M.Map ArithGate Int -> Int -> StateT (Int, Ref) m ()
    check ref degs depth
      | depth > maxDepth = return ()
      | otherwise = do
          existingDeg <- gets fst
          let deg = maximum (0 : M.elems degs)
          when (deg > existingDeg) $ do
              traceM (printf "deg=%d" deg)
              put (deg, ref)


-- find the first subcircuit that satisfies some predicate
findFirst :: (Circuit ArithGate -> Bool) -> Circuit ArithGate -> Maybe Ref
findFirst pred c = listToMaybe $ catMaybes $ runIdentity (foldCircM eval c)
  where
    eval _ ref [xref, yref] = return $
        if isJust xref then xref else
        if isJust yref then yref else
        if pred (cheapSlice ref c)
           then Just ref
           else Nothing
    eval _ _ _ = return Nothing

hasHighSingleVarDeg :: Int -> Circuit ArithGate -> Bool
hasHighSingleVarDeg deg c = any ((>= deg) . fromIntegral) (nonPublicDegs c)

nonPublicDegs :: Circuit ArithGate -> [Integer]
nonPublicDegs c = map (maxVarDegree c) ids
  where
    allIds = map (Input . InputId) [0 .. ninputs c-1]
    ok (Secret _ ) = False
    ok (Const _)   = True
    ok (Input _ )  = True
    ids = filter ok allIds

-- find the highest degree subcircuit within a given depth
findFirstHSVD :: Int -> Int -> Circuit ArithGate -> [Ref]
findFirstHSVD maxDepth minDeg c = execWriter (foldCircM eval c)
  where
    eval :: ArithGate -> Ref -> [(M.Map ArithGate Int, Int)] -> Writer [Ref] (M.Map ArithGate Int, Int)
    eval (ArithMul _ _) ref [(xdegs, xdepth), (ydegs, ydepth)] = do
        let degs  = M.unionWith (+) xdegs ydegs
            depth = max xdepth ydepth + 1
        check ref degs depth
        return (degs, depth)

    eval (ArithAdd _ _) ref [(xdegs, xdepth), (ydegs, ydepth)] = do
        let degs  = M.unionWith (max) xdegs ydegs
            depth = max xdepth ydepth + 1
        check ref degs depth
        return (degs, depth)

    eval (ArithSub _ _) ref [(xdegs, xdepth), (ydegs, ydepth)] = do
        let degs  = M.unionWith (max) xdegs ydegs
            depth = max xdepth ydepth + 1
        check ref degs depth
        return (degs, depth)

    eval op@(ArithBase (Input   _)) _ _ = return (M.singleton op 1, 0)
    eval op@(ArithBase (Secret id)) _ _ = return (M.singleton op 1, 0)
    eval op@(ArithBase (Const  id)) _ _ = return (M.empty         , 0)

    eval _ _ _ = undefined

    check :: Ref -> M.Map ArithGate Int -> Int -> Writer [Ref] ()
    check ref degs depth
      | depth > maxDepth = return ()
      | otherwise = when (any (>= minDeg) (M.elems degs)) (tell [ref])

-- get a subcircuit using an intermediate ref as an output ref
slice :: Ref -> Circuit ArithGate -> Circuit ArithGate
slice ref c = B.buildCircuit $ do
    out <- foldCircRefM eval c ref
    B.output out
  where
    eval (ArithAdd _ _) _ [x,y] = B.circAdd x y
    eval (ArithSub _ _) _ [x,y] = B.circSub x y
    eval (ArithMul _ _) _ [x,y] = B.circMul x y
    eval (ArithBase (Input  i)) _ _ = B.inputN i
    eval (ArithBase (Secret i)) _ _ = B.secret (getSecret c i)
    eval (ArithBase (Const  i)) _ _ = B.constant (getConst c i)
    eval _ _ _ = error "[slice] oops"

-- get a slice without rebuilding!
cheapSlice :: Ref -> Circuit ArithGate -> Circuit ArithGate
cheapSlice root = circ_outputs .~ V.singleton root

-- replace subcircuit ending at loc with c2 in c1
patch :: Ref -> Circuit ArithGate -> Circuit ArithGate -> Circuit ArithGate
patch loc c1 c2
  | noutputs c2 /= 1 = error "[patch] expected c2 to have only one output"
  | otherwise = B.buildCircuit $ do
    xs <- B.inputs (ninputs c1)
    ys1 <- B.exportConsts c1
    ys2 <- B.exportConsts c2
    zs1 <- B.exportSecrets c1
    zs2 <- B.exportSecrets c2

    -- when we get to loc, eval c2 as subcircuit and return that output
    let catch ref other = if ref == loc
                             then head <$> B.subcircuit' c2 xs (ys1 ++ zs2) (zs1 ++ zs2)
                             else other

    let eval (ArithAdd _ _) ref [x,y] = catch ref $ B.circAdd x y
        eval (ArithSub _ _) ref [x,y] = catch ref $ B.circSub x y
        eval (ArithMul _ _) ref [x,y] = catch ref $ B.circMul x y
        eval (ArithBase (Input  i)) ref _ = catch ref $ B.inputN i
        eval (ArithBase (Const  i)) ref _ = catch ref $ B.constant (getConst c1 i)
        eval (ArithBase (Secret i)) ref _ = catch ref $ B.secret_n i
        eval _ _ _ = error "[slice] oops"

    outs <- foldCircM eval c1
    B.outputs outs

foldConsts :: Circuit ArithGate -> Circuit ArithGate
foldConsts c = B.buildCircuit $ do
    _  <- B.inputs (ninputs c)
    _  <- B.exportConsts c
    _  <- B.exportSecrets c
    zs <- foldCircM eval c
    B.outputs (map fst zs)

  where
    eval (ArithAdd _ _) _ [x,y] = case (snd x, snd y) of
        (Just a, Just b) -> do z <- B.constant (a+b); return (z, Just (a+b))
        (Just 0, _     ) -> return y
        (_     , Just 0) -> return x
        (_     , _     ) -> do z <- B.circAdd (fst x) (fst y); return (z, Nothing)

    eval (ArithSub _ _) _ [x,y] = case (snd x, snd y) of
        (Just a, Just b) -> do z <- B.constant (a-b); return (z, Just (a-b))
        (_     , Just 0) -> return x
        (_     , _     ) -> do z <- B.circSub (fst x) (fst y); return (z, Nothing)

    eval (ArithMul _ _) _ [x,y] = case (snd x, snd y) of
        (Just a, Just b) -> do z <- B.constant (a*b); return (z, Just (a*b))
        (Just 1, _     ) -> return y
        (_     , Just 1) -> return x
        (_     , _     ) -> do z <- B.circMul (fst x) (fst y); return (z, Nothing)

    eval (ArithBase (Input  i)) _ _ = do z <- B.inputN i; return (z, Nothing)
    eval (ArithBase (Const  i)) _ _ = do z <- B.constant (getConst c i); return (z, Just (getConst c i))
    eval (ArithBase (Secret i)) _ _ = do z <- B.secret (getSecret c i); return (z, Nothing)

    eval _ _ _ = error "[foldConsts] oops"

-- remove unused gates
cleanup :: Circuit ArithGate -> Circuit ArithGate
cleanup c = B.buildCircuit $ do
    inps <- B.inputs (ninputs c)
    outs <- B.subcircuit c inps
    B.outputs outs

flattenRec :: Circuit ArithGate -> IO (Circuit ArithGate)
flattenRec c = outerLoop maxDepth c
  where
    maxDepth = 14

    outerLoop 1      c = return c
    outerLoop minDeg c = innerLoop minDeg (findFirstHSVD maxDepth minDeg c) c

    innerLoop minDeg []        c = outerLoop (minDeg - 1) c
    innerLoop minDeg (root:rs) c = do
        let sub = foldConsts (cheapSlice root c)
        printf "[flattenRec]\n\troot=%d, total_gates=%d, total_nconsts=%d, total_degree=%d\n\tsub_gates=%d, sub_nin=%d, sub_deg=%d, sub_depth=%d\n"
                (getRef root) (nwires c) (nconsts c) (circDegree c)
                (nwires sub) (ninputs sub) (circDegree sub) (depth sub)
        sub' <- flatten sub
        printf "[flattenRec] flattened subcircuit degree: %d\n" (circDegree sub')
        let c' = patch root c sub'
        if circDegree sub' < circDegree sub then
            outerLoop maxDepth (foldConsts c')
        else
            innerLoop minDeg rs c

-- push multiplications down toward the inputs
pushDown :: Circuit ArithGate -> Circuit ArithGate
pushDown c = B.buildCircuit $ do
    _ <- B.inputs (ninputs c)
    _ <- B.exportConsts c
    zs <- foldCircM eval c
    B.outputs (map fst zs)

  where
    eval (ArithAdd _ _) _ [x,y] = do
        z <- B.circAdd (fst x) (fst y)
        return (z, Just (ArithAdd (fst x) (fst y)))

    eval (ArithSub _ _) _ [x,y] = do
        z <- B.circSub (fst x) (fst y)
        return (z, Just (ArithSub (fst x) (fst y)))

    eval (ArithMul _ _) _ [x,y] = case (snd x, snd y) of
        (Just (ArithAdd xa xb), Just (ArithAdd ya yb)) -> do
            a <- B.circMul xa ya
            b <- B.circMul xa yb
            c <- B.circMul xb ya
            d <- B.circMul xb yb
            e <- B.circAdd a b
            f <- B.circAdd c d
            g <- B.circAdd e f
            return (g, Just (ArithAdd e f))

        -- (xa - xb)(ya - yb) = xa*ya - xa*yb - xb*ya + xb*yb
        --                    = (xa*ya - xa*yb) + (xb*yb - xb*ya)
        (Just (ArithSub xa xb), Just (ArithSub ya yb)) -> do
            a <- B.circMul xa ya
            b <- B.circMul xa yb
            c <- B.circMul xb ya
            d <- B.circMul xb yb
            e <- B.circSub a b
            f <- B.circSub d c
            g <- B.circAdd e f
            return (g, Just (ArithAdd e f))

        -- (xa + xb)(ya - yb) = xa*ya - xa*yb + xb*ya - xb*yb
        --                    = (xa*ya - xa*yb) + (xb*ya - xb*yb)
        (Just (ArithAdd xa xb), Just (ArithSub ya yb)) -> do
            a <- B.circMul xa ya
            b <- B.circMul xa yb
            c <- B.circMul xb ya
            d <- B.circMul xb yb
            e <- B.circSub a b
            f <- B.circSub c d
            g <- B.circAdd e f
            return (g, Just (ArithAdd e f))

        -- (xa - xb)(ya + yb) = xa*ya + xa*yb - xb*ya - xb*yb
        --                    = (xa*ya + xa*yb) - (xb*ya + xb*yb)
        (Just (ArithSub xa xb), Just (ArithAdd ya yb)) -> do
            a <- B.circMul xa ya
            b <- B.circMul xa yb
            c <- B.circMul xb ya
            d <- B.circMul xb yb
            e <- B.circAdd a b
            f <- B.circAdd c d
            g <- B.circSub e f
            return (g, Just (ArithSub e f))

        (Just (ArithAdd xa xb), _) -> do
            a <- B.circMul (fst y) xa
            b <- B.circMul (fst y) xb
            z <- B.circAdd a b
            return (z, Just (ArithAdd a b))

        (Just (ArithSub xa xb), _) -> do
            a <- B.circMul (fst y) xa
            b <- B.circMul (fst y) xb
            z <- B.circSub a b
            return (z, Just (ArithSub a b))

        (_ , Just (ArithAdd ya yb)) -> do
            a <- B.circMul (fst x) ya
            b <- B.circMul (fst x) yb
            z <- B.circAdd a b
            return (z, Just (ArithAdd a b))

        (_ , Just (ArithSub ya yb)) -> do
            a <- B.circMul (fst x) ya
            b <- B.circMul (fst x) yb
            z <- B.circSub a b
            return (z, Just (ArithSub a b))

        (_ , _) -> do
            z <- B.circMul (fst x) (fst y)
            return (z, Nothing)

    eval (ArithBase (Input i)) _ _ = do
        z <- B.inputN i
        return (z, Nothing)

    eval (ArithBase (Secret i)) _ _ = do
        z <- B.secret (getSecret c i)
        return (z, Nothing)

    eval _ _ _ = error "[pushDown] oops"
