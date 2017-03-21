{-# LANGUAGE ScopedTypeVariables #-}
module Circuit.Optimizer where

import Circuit
import qualified Circuit.Format.Sexp as Sexp
import Text.Printf
import System.Process
import Control.Monad.State
import Debug.Trace
import qualified Data.Map as M
import qualified Circuit.Builder as B

circToSage :: Circuit -> [String]
circToSage c = foldCirc eval c
  where
    eval (OpAdd _ _) [x,y] = printf "(%s) + (%s)" x y
    eval (OpSub _ _) [x,y] = printf "(%s) - (%s)" x y
    eval (OpMul _ _) [x,y] = printf "(%s) * (%s)" x y
    eval (OpInput  i) []   = printf "var('x%d')" (getId i)
    eval (OpSecret i) []   = if publicConst c i
                                then show (getSecret c i)
                                else printf "var('y%d')" (getId i)
    eval op args  = error ("[circToSexp] weird input: " ++ show op ++ " " ++ show args)

callSage :: Int -> String -> IO Circuit
callSage ninputs s = do
    r <- readProcess "./scripts/poly-sage.sage" [] s
    -- printf "[callSage] output=\"%s\"\n" r
    return $ Sexp.parse ninputs r

flatten :: Circuit -> IO Circuit
flatten c = merge <$> mapM (callSage (ninputs c)) (circToSage c)

-- merge circuits to use the same inputs, consts, and intermediate gates
merge :: [Circuit] -> Circuit
merge cs = B.buildCircuit $ do
    xs   <- B.inputs (ninputs (head cs))
    outs <- concat <$> mapM (flip B.subcircuit xs) cs
    B.outputs outs

-- find the highest degree subcircuit within a given depth
find :: Int -> Circuit -> Ref
find maxDepth c = snd $ execState (foldCircM eval c) (0, Ref 0)
  where
    eval (OpAdd _ _) ref [(xdegs, xdepth), (ydegs, ydepth)] = do
        let degs  = M.unionWith (max) xdegs ydegs
            depth = max xdepth ydepth + 1
        check ref degs depth
        return (degs, depth)

    eval (OpSub _ _) ref [(xdegs, xdepth), (ydegs, ydepth)] = do
        let degs  = M.unionWith (max) xdegs ydegs
            depth = max xdepth ydepth + 1
        check ref degs depth
        return (degs, depth)

    eval (OpMul _ _) ref [(xdegs, xdepth), (ydegs, ydepth)] = do
        let degs  = M.unionWith (+) xdegs ydegs
            depth = max xdepth ydepth + 1
        check ref degs depth
        return (degs, depth)

    eval op@(OpInput _)   _ _ = return (M.singleton op 1, 0)

    eval op@(OpSecret id) _ _ = if publicConst c id
                                   then return (M.empty         , 0)
                                   else return (M.singleton op 1, 0)

    eval _ _ _ = undefined

    check :: Monad m => Ref -> M.Map Op Int -> Int -> StateT (Int, Ref) m ()
    check ref degs depth
      | depth > maxDepth = return ()
      | otherwise = do
          existingDeg <- gets fst
          let deg = maximum (0 : M.elems degs)
          when (deg > existingDeg) $ do
              traceM (printf "deg=%d" deg)
              put (deg, ref)

-- get a subcircuit using an intermediate ref as an output ref
slice :: Ref -> Circuit -> Circuit
slice ref c = B.buildCircuit $ do
    out <- foldCircRefM eval c ref
    B.output out
  where
    eval (OpAdd _ _) _ [x,y] = B.circAdd x y
    eval (OpSub _ _) _ [x,y] = B.circSub x y
    eval (OpMul _ _) _ [x,y] = B.circMul x y
    eval (OpInput  i) _ _ = B.input_n i
    eval (OpSecret i) _ _ = let sec = getSecret c i in
                                if publicConst c i
                                   then B.constant sec
                                   else B.secret sec
    eval _ _ _ = error "[slice] oops"

-- replace subcircuit ending at loc with c2 in c1
patch :: Ref -> Circuit -> Circuit -> Circuit
patch loc c1 c2
  | noutputs c2 /= 1 = error "[patch] expected c2 to have only one output"
  | otherwise = B.buildCircuit $ do
    xs <- B.inputs (ninputs c1)
    _  <- B.exportSecrets c1

    -- when we get to loc, eval c2 as subcircuit and return that output
    let catch ref other = if ref /= loc then other else head <$> B.subcircuit c2 xs
        eval (OpAdd _ _) ref [x,y] = catch ref $ B.circAdd x y
        eval (OpSub _ _) ref [x,y] = catch ref $ B.circSub x y
        eval (OpMul _ _) ref [x,y] = catch ref $ B.circMul x y
        eval (OpInput  i) ref _ = catch ref $ B.input_n i
        eval (OpSecret i) ref _ = catch ref $ B.secret_n i
        eval _ _ _ = error "[slice] oops"

    outs <- foldCircM eval c1
    B.outputs outs

foldConsts :: Circuit -> Circuit
foldConsts c = B.buildCircuit $ do
    _ <- B.inputs (ninputs c)
    _ <- B.exportSecrets c
    zs <- foldCircM eval c
    B.outputs (map fst zs)

  where
    eval (OpAdd _ _) _ [x,y] = case (snd x, snd y) of
        (Just a, Just b) -> do z <- B.constant (a+b); return (z, Just (a+b))
        (Just 0, _     ) -> return y
        (_     , Just 0) -> return x
        (_     , _     ) -> do z <- B.circAdd (fst x) (fst y); return (z, Nothing)

    eval (OpSub _ _) _ [x,y] = case (snd x, snd y) of
        (Just a, Just b) -> do z <- B.constant (a-b); return (z, Just (a-b))
        (_     , Just 0) -> return x
        (_     , _     ) -> do z <- B.circSub (fst x) (fst y); return (z, Nothing)

    eval (OpMul _ _) _ [x,y] = case (snd x, snd y) of
        (Just a, Just b) -> do z <- B.constant (a*b); return (z, Just (a*b))
        (Just 1, _     ) -> return y
        (_     , Just 1) -> return x
        (_     , _     ) -> do z <- B.circMul (fst x) (fst y); return (z, Nothing)

    eval (OpInput  i) _ _ = do z <- B.input_n i; return (z, Nothing)
    eval (OpSecret i) _ _ = do
        let sec = if publicConst c i then Just (getSecret c i) else Nothing
        z <- B.secret_n i
        return (z, sec)

    eval _ _ _ = error "[foldConsts] oops"

-- remove unused gates
cleanup :: Circuit -> Circuit
cleanup c = B.buildCircuit $ do
    inps <- B.inputs (ninputs c)
    outs <- B.subcircuit c inps
    B.outputs outs

flattenRec :: Int -> Circuit -> IO Circuit
flattenRec maxDepth c = do
    let root = find maxDepth c
        sub  = slice root c
    printf "[flattenRec] root=%d, nin=%d, deg=%d, depth=%d, total_degree=%d\n"
            (getRef root) (ninputs sub) (circDegree sub) (depth sub) (circDegree c)
    -- sub' <- catch (flatten sub) (\(_ :: SomeException) -> writeFile "/tmp/oops" (unlines (circToSage sub)) >> undefined)
    sub' <- flatten sub
    printf "[flattenRec] flattened subcircuit degree: %d\n" (circDegree sub')
    let c' = patch root c sub'
    if circDegree sub' < circDegree sub then
        flattenRec maxDepth (foldConsts c')
    else
        return c'
