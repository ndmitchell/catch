
module Transform.Rewrite(exprTweak, funcTweak) where

import Transform.Type

import Control.Exception
import Data.List


exprTweak = exprTweaks [basicExpr]

funcTweak = funcTweaks [deadArg, lambdaRaise]


---------------------------------------------------------------------
-- BASIC RULES


basicExpr :: ExprTweak

-- structural simplifications
basicExpr ihite (Apply x []) = Change x
basicExpr ihite (Lambda [] x) = Change x
basicExpr ihite (Call "_" []) = Change Unknown

-- (Cons a b).head ==> a
basicExpr ihite (Sel (Make name args) arg) =
		assert (ctorName x == name) $ Change $ args !! cargPos x
	where x = getCArg ihite arg

-- case Nil of {Nil -> a; Cons -> b} ==> a
basicExpr ihite (Case (Make name args) xs) = Change $ head ys
	where ys = [b | (a,b) <- xs, a == name]

-- Apply (Lambda free expr) args  ==> expr[free/args]
basicExpr ihite (Apply (Lambda (f:ree) expr) (a:rgs)) =
	Change $ Apply (Lambda ree (mapIExpr rep expr)) rgs
	where
		rep (Var i) | i == f = a
		rep x = x

basicExpr ihite _ = None



---------------------------------------------------------------------
-- DEAD ARGUMENT ELIMINATION

-- f a b = expr_on_b  ==>  f b = expr_on_b
deadArg :: FuncTweak
deadArg ihite (Func name args body)
		| not (null deads)
		= Just (Func name (args \\ deads) body, f)
	where
		deads = args \\ [i | Var i <- allIExpr body]
		
		f (Call nam xs) | nam == name = Call nam [x | (i,x) <- zip [0..] xs, i `notElem` deads]
		f x = x

deadArg ihite _ = Nothing


---------------------------------------------------------------------
-- LAMBDA RAISE

-- f a = Lambda n b ==> f a n = b
lambdaRaise :: FuncTweak
lambdaRaise ihite (Func name args (Lambda xs body))
		= Just (Func name (args ++ xs) body, f)
	where
		f o@(Call nam xs) | nam == name = Lambda free (Call nam (xs ++ map Var free))
			where free = take (length xs) $ freshFree o
		f x = x

lambdaRaise _ _ = Nothing
