module Circuit.Optimizer where

import Circuit
import qualified Circuit.Format.Sexp as Sexp
import Text.Printf
import System.Process

circToSexp :: Circuit -> [String]
circToSexp c = foldCirc eval c
  where
    eval (OpAdd _ _) [x,y] = printf "Add(%s, %s)" x y
    eval (OpSub _ _) [x,y] = printf "Add(%s, Mul(Integer(-1), %s))" x y
    eval (OpMul _ _) [x,y] = printf "Mul(%s, %s)" x y
    eval (OpInput  i) []   = printf "Symbol('x%d')" (getId i)
    eval (OpSecret i) []   = printf "Symbol('y%d')" (getId i)
    eval op args  = error ("[circToSexp] weird input: " ++ show op ++ " " ++ show args)

circToSage :: Circuit -> [String]
circToSage c = foldCirc eval c
  where
    eval (OpAdd _ _) [x,y] = printf "(%s) + (%s)" x y
    eval (OpSub _ _) [x,y] = printf "(%s) - (%s)" x y
    eval (OpMul _ _) [x,y] = printf "(%s) * (%s)" x y
    eval (OpInput  i) []   = printf "var('x%d')" (getId i)
    eval (OpSecret i) []   = printf "var('y%d')" (getId i)
    eval op args  = error ("[circToSexp] weird input: " ++ show op ++ " " ++ show args)

flattenPolynomial :: Circuit -> IO Circuit
flattenPolynomial c
  | ninputs c /= 1 = error "[flattenPolynomial] only single bit output supported"
  | otherwise = do
    let s = head (circToSage c)
    r <- readProcess "./scripts/poly-sage.sage" [] s
    return $ Sexp.parseNInputs1 (ninputs c) r
