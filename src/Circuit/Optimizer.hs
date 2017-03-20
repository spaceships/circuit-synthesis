module Circuit.Optimizer where

import Circuit
import qualified Circuit.Format.Sexp as Sexp
import Text.Printf
import System.Process
import Control.Monad.State
import qualified Circuit.Builder as B

circToSage :: Circuit -> [String]
circToSage c = foldCirc eval c
  where
    eval (OpAdd _ _) [x,y] = printf "(%s) + (%s)" x y
    eval (OpSub _ _) [x,y] = printf "(%s) - (%s)" x y
    eval (OpMul _ _) [x,y] = printf "(%s) * (%s)" x y
    eval (OpInput  i) []   = printf "var('x%d')" (getId i)
    eval (OpSecret i) []   = if publicConst i c
                                then show (getSecret c i)
                                else printf "var('y%d')" (getId i)
    eval op args  = error ("[circToSexp] weird input: " ++ show op ++ " " ++ show args)

callSage :: Int -> String -> IO Circuit
callSage ninputs s = do
    r <- readProcess "./scripts/poly-sage.sage" [] s
    return $ Sexp.parse ninputs r

flatten :: Circuit -> IO Circuit
flatten c = merge <$> mapM (callSage (ninputs c)) (circToSage c)

-- merge circuits to use the same inputs, consts, and intermediate gates
merge :: [Circuit] -> Circuit
merge cs = B.buildCircuit $ do
    xs   <- B.inputs (ninputs (head cs))
    outs <- concat <$> mapM (flip B.subcircuit xs) cs
    B.outputs outs

-- remove unused gates
cleanup :: Circuit -> Circuit
cleanup c = B.buildCircuit $ do
    inps <- B.inputs (ninputs c)
    outs <- B.subcircuit c inps
    B.outputs outs

-- find the highest degree subcircuit within a given depth
find :: Int -> Circuit -> Ref
find maxDepth c = snd $ execState (foldCircM eval c) (0, Ref 0)
  where
    eval (OpAdd _ _) ref [(xdeg, xdepth), (ydeg, ydepth)] = do
        let deg   = max xdeg ydeg
            depth = max xdepth ydepth + 1
        check ref deg depth
        return (deg, depth)
    eval (OpSub _ _) ref [(xdeg, xdepth), (ydeg, ydepth)] = do
        let deg   = max xdeg ydeg
            depth = max xdepth ydepth + 1
        check ref deg depth
        return (deg, depth)
    eval (OpMul _ _) ref [(xdeg, xdepth), (ydeg, ydepth)] = do
        let deg   = xdeg + ydeg
            depth = max xdepth ydepth + 1
        check ref deg depth
        return (deg, depth)
    eval _ _ _ = return (1,1)

    check :: Monad m => Ref -> Int -> Int -> StateT (Int, Ref) m ()
    check ref deg depth
      | depth > maxDepth = return ()
      | otherwise = do
          existingDeg <- gets fst
          when (deg > existingDeg) $ put (deg, ref)

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
    eval (OpSecret i) _ _ = if publicConst i c then B.constant (getSecret c i)
                                               else B.secret (getSecret c i)
    eval _ _ _ = error "[slice] oops"

-- replace subcircuit ending at loc with c2 in c1
patch :: Ref -> Circuit -> Circuit -> Circuit
patch loc c1 c2
  | noutputs c2 /= 1 = error "[patch] expected c2 to have only one output"
  | otherwise = B.buildCircuit $ do
    xs <- B.inputs (ninputs c1)
    _  <- B.exportSecrets c1

    let catch ref other = if ref /= loc then other else head <$> B.subcircuit c2 xs

        eval (OpAdd _ _) ref [x,y] = catch ref $ B.circAdd x y
        eval (OpSub _ _) ref [x,y] = catch ref $ B.circSub x y
        eval (OpMul _ _) ref [x,y] = catch ref $ B.circMul x y
        eval (OpInput  i) ref _ = catch ref $ B.input_n i
        eval (OpSecret i) ref _ = catch ref $ B.secret_n i
        eval _ _ _ = error "[slice] oops"

    outs <- foldCircM eval c1
    B.outputs outs

-- when we get to loc, eval c2 as subcircuit and return that output

-- foldCircM :: Monad m => (Op -> Ref -> [a] -> m a) -> Circuit -> m [a]
