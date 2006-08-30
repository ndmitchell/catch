
module Transform.Rewrite(exprTweak, funcTweak, funcCreate) where

import Transform.Type

import Control.Exception
import Data.List


exprTweak = joinTweaks [basicExpr]

funcTweak = joinTweaks [deadArg, lambdaRaise]

funcCreate = joinTweaks [defuncExpr]


---------------------------------------------------------------------
-- BASIC RULES


basicExpr :: ExprTweak

-- structural simplifications
basicExpr ihite (Apply x []) = Just x
basicExpr ihite (Lambda [] x) = Just x
basicExpr ihite (Call "_" []) = Just Unknown

-- (Cons a b).head ==> a
basicExpr ihite (Sel (Make name args) arg) =
		assert (ctorName x == name) $ Just $ args !! cargPos x
	where x = getCArg ihite arg

-- case Nil of {Nil -> a; Cons -> b} ==> a
basicExpr ihite (Case (Make name args) xs) = Just $ head ys
	where ys = [b | (a,b) <- xs, a == name]

-- Apply (Lambda free expr) args  ==> expr[free/args]
basicExpr ihite (Apply (Lambda (f:ree) expr) (a:rgs)) =
	Just $ Apply (Lambda ree (replaceFree [(f,a)] expr)) rgs

basicExpr ihite _ = Nothing



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


---------------------------------------------------------------------
-- DEFUNCTIONALISE FUNCTION

-- f (pre:Lambda args (Call x xs):pre)  ==>
--     f' pre:fv:post,  f'=f[arg/Lambda args (Call x xs)]
defuncExpr :: FuncCreate
defuncExpr ihite (Call name xs) | any isHO xs
		= Just (newfunc, newexpr)
	where
		(pre,spec:post) = break isHO xs
		fvSpec = collectFree spec
		
		newexpr name2 = Call name2 (pre ++ map Var fvSpec ++ post)
		
		Func _ args body = getFunc ihite name
		newargs = take (length fvSpec) $ freshFree body \\ args
		
		lpre = length pre
		newfunc = Func name
			(take lpre args ++ newargs ++ drop (lpre+1) args)
			(replaceFree [(args!!lpre, replaceFree (zip fvSpec (map Var newargs)) spec)] body)
		
		isHO (Lambda _ (Call _ _)) = True
		isHO _ = False
