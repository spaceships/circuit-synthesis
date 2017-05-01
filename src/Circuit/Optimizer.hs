{-# LANGUAGE ScopedTypeVariables #-}
module Circuit.Optimizer where

import Circuit
import qualified Circuit.Format.Sexp as Sexp
import Text.Printf
import System.Process
import Control.Monad.State
import Control.Monad.Identity
import Control.Monad.Except
import Data.Maybe (isJust, catMaybes, listToMaybe)
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
        let degs  = M.unionWith max xdegs ydegs
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


-- find the first subcircuit that satisfies some predicate
findFirst :: (Circuit -> Bool) -> Circuit -> Maybe Ref
findFirst pred c = listToMaybe $ catMaybes $ runIdentity (foldCircM eval c)
  where
    eval _ ref [xref, yref] = return $
        if isJust xref then xref else
        if isJust yref then yref else
        if pred (cheapSlice ref c)
           then Just ref
           else Nothing
    eval _ _ _ = return Nothing

hasHighSingleVarDeg :: Int -> Circuit -> Bool
hasHighSingleVarDeg deg c = any ((>= deg) . fromIntegral) (nonPublicDegs c)

nonPublicDegs :: Circuit -> [Integer]
nonPublicDegs c = map (varDegree c) ids
  where
    allIds = map (OpInput . Id) [0 .. ninputs c-1]

    ok (OpSecret id) = publicConst c id
    ok (OpInput  _ ) = True
    ok _             = undefined

    ids = filter ok allIds


-- find the highest degree subcircuit within a given depth
findFirstHSVD :: Int -> Int -> Circuit -> Maybe Ref
findFirstHSVD maxDepth minDeg c = case runExcept (foldCircM eval c) of
    Left ref -> Just ref
    Right _  -> Nothing

  where
    eval (OpMul _ _) ref [(xdegs, xdepth), (ydegs, ydepth)] = do
        let degs  = M.unionWith (+) xdegs ydegs
            depth = max xdepth ydepth + 1
        check ref degs depth
        return (degs, depth)

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

    eval op@(OpInput _)   _ _ = return (M.singleton op 1, 0)

    eval op@(OpSecret id) _ _ = if publicConst c id
                                   then return (M.empty         , 0)
                                   else return (M.singleton op 1, 0)

    eval _ _ _ = undefined

    check ref degs depth
      | depth > maxDepth = return ()
      | otherwise = when (any (>= minDeg) (M.elems degs)) (throwError ref)

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

-- get a slice without rebuilding!
cheapSlice :: Ref -> Circuit -> Circuit
cheapSlice root c = c { circ_outputs = [root] }

-- replace subcircuit ending at loc with c2 in c1
patch :: Ref -> Circuit -> Circuit -> Circuit
patch loc c1 c2
  | noutputs c2 /= 1 = error "[patch] expected c2 to have only one output"
  | otherwise = B.buildCircuit $ do
    xs <- B.inputs (ninputs c1)
    ys <- B.exportSecrets c1

    -- when we get to loc, eval c2 as subcircuit and return that output
    let catch ref other = if ref == loc
                             then head <$> B.subcircuit' c2 xs ys
                             else other

    let eval (OpAdd _ _) ref [x,y] = catch ref $ B.circAdd x y
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

flattenRec :: Circuit -> IO Circuit
flattenRec c = loop maxDepth c
  where
    maxDepth = 14

    loop 0      c = return c
    loop minDeg c = case findFirstHSVD maxDepth minDeg c of
        Nothing   -> loop (minDeg - 1) c
        Just root -> do
            let sub = foldConsts (cheapSlice root c)
            printf "[flattenRec]\n\troot=%d, total_gates=%d, total_nconsts=%d, total_degree=%d\n\tsub_gates=%d, sub_nin=%d, sub_deg=%d, sub_depth=%d\n"
                    (getRef root) (ngates c) (nconsts c) (circDegree c)
                    (ngates sub) (ninputs sub) (circDegree sub) (depth sub)
            sub' <- flatten sub
            printf "[flattenRec] flattened subcircuit degree: %d\n" (circDegree sub')
            let c' = patch root c sub'
            if circDegree sub' < circDegree sub then
                -- loop minDeg (foldConsts (pushDown c'))
                loop minDeg (foldConsts c')
            else
                return c'

-- push multiplications down toward the inputs
pushDown :: Circuit -> Circuit
pushDown c = B.buildCircuit $ do
    _ <- B.inputs (ninputs c)
    _ <- B.exportSecrets c
    zs <- foldCircM eval c
    B.outputs (map fst zs)

  where
    eval (OpAdd _ _) _ [x,y] = do
        z <- B.circAdd (fst x) (fst y)
        return (z, Just (OpAdd (fst x) (fst y)))

    eval (OpSub _ _) _ [x,y] = do
        z <- B.circSub (fst x) (fst y)
        return (z, Just (OpSub (fst x) (fst y)))

    eval (OpMul _ _) _ [x,y] = case (snd x, snd y) of
        (Just (OpAdd xa xb), Just (OpAdd ya yb)) -> do
            a <- B.circMul xa ya
            b <- B.circMul xa yb
            c <- B.circMul xb ya
            d <- B.circMul xb yb
            e <- B.circAdd a b
            f <- B.circAdd c d
            g <- B.circAdd e f
            return (g, Just (OpAdd e f))

        -- (xa - xb)(ya - yb) = xa*ya - xa*yb - xb*ya + xb*yb
        --                    = (xa*ya - xa*yb) + (xb*yb - xb*ya)
        (Just (OpSub xa xb), Just (OpSub ya yb)) -> do
            a <- B.circMul xa ya
            b <- B.circMul xa yb
            c <- B.circMul xb ya
            d <- B.circMul xb yb
            e <- B.circSub a b
            f <- B.circSub d c
            g <- B.circAdd e f
            return (g, Just (OpAdd e f))

        -- (xa + xb)(ya - yb) = xa*ya - xa*yb + xb*ya - xb*yb
        --                    = (xa*ya - xa*yb) + (xb*ya - xb*yb)
        (Just (OpAdd xa xb), Just (OpSub ya yb)) -> do
            a <- B.circMul xa ya
            b <- B.circMul xa yb
            c <- B.circMul xb ya
            d <- B.circMul xb yb
            e <- B.circSub a b
            f <- B.circSub c d
            g <- B.circAdd e f
            return (g, Just (OpAdd e f))

        -- (xa - xb)(ya + yb) = xa*ya + xa*yb - xb*ya - xb*yb
        --                    = (xa*ya + xa*yb) - (xb*ya + xb*yb)
        (Just (OpSub xa xb), Just (OpAdd ya yb)) -> do
            a <- B.circMul xa ya
            b <- B.circMul xa yb
            c <- B.circMul xb ya
            d <- B.circMul xb yb
            e <- B.circAdd a b
            f <- B.circAdd c d
            g <- B.circSub e f
            return (g, Just (OpSub e f))

        (Just (OpAdd xa xb), _) -> do
            a <- B.circMul (fst y) xa
            b <- B.circMul (fst y) xb
            z <- B.circAdd a b
            return (z, Just (OpAdd a b))

        (Just (OpSub xa xb), _) -> do
            a <- B.circMul (fst y) xa
            b <- B.circMul (fst y) xb
            z <- B.circSub a b
            return (z, Just (OpSub a b))

        (_ , Just (OpAdd ya yb)) -> do
            a <- B.circMul (fst x) ya
            b <- B.circMul (fst x) yb
            z <- B.circAdd a b
            return (z, Just (OpAdd a b))

        (_ , Just (OpSub ya yb)) -> do
            a <- B.circMul (fst x) ya
            b <- B.circMul (fst x) yb
            z <- B.circSub a b
            return (z, Just (OpSub a b))

        (_ , _) -> do
            z <- B.circMul (fst x) (fst y)
            return (z, Nothing)

    eval (OpInput i) _ _ = do
        z <- B.input_n i
        return (z, Nothing)

    eval (OpSecret i) _ _ = do
        z <- B.secret_n i
        return (z, Nothing)

    eval _ _ _ = error "[foldConsts] oops"
