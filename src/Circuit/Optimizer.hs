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
flatten c = B.mergeCircuits <$> mapM (callSage (ninputs c)) (circToSage c)

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
    eval (OpSecret i) _ _ = B.secret_n i
    eval _ _ _ = error "[slice] oops"
