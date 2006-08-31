
module Transform.Rewrite(exprTweak, funcTweak, funcCreate) where

import Transform.Type

import Control.Exception
import Data.List


exprTweak = joinTweaks [basicExpr, inlineTuple, inlineExpr]

funcTweak = joinTweaks [deadArg, lambdaRaise]

funcCreate = joinTweaks [defuncExpr, dedictExpr]


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
		f o@(Call nam params) | nam == name = Lambda free (Call nam (params ++ map Var free))
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
		
		-- UNSURE IF THIS IS CORRECT
		-- WHAT IF ARBITRARY EXPRESSION, LEADS TO NON-TERM?
		-- Potentially unsafe generalisation
		-- isHO (Lambda _ _) = True

		isHO (Lambda _ (Call _ _)) = True
		isHO (Lambda _ Unknown) = True
		isHO _ = False

defuncExpr _ _ = Nothing


---------------------------------------------------------------------
-- DICTIONARY REMOVAL

-- remove dictionaries, see MonadFail2 for an example

isTuple (Make ('T':'u':'p':_) _) = True
isTuple _ = False


-- f, where f = Tup _ _ ==> Tup _ _
inlineTuple :: ExprTweak
inlineTuple ihite (Call name []) | isTuple body = Just body
	where body = funcExpr $ getFunc ihite name
inlineTuple _ _ = Nothing


dedictExpr :: FuncCreate
dedictExpr ihite (Call name xs) | any isTuple xs
		= Just (newfunc, newexpr)
	where
		(pre,Make tupn tupArgs:post) = break isTuple xs
		
		newexpr newname = Call newname (pre ++ tupArgs ++ post)
		
		Func _ args body = getFunc ihite name
		newargs = take (length tupArgs) $ freshFree body \\ args

		lpre = length pre
		newfunc = Func name
			(take lpre args ++ newargs ++ drop (lpre+1) args)
			(replaceFree [(args!!lpre,Make tupn (map Var newargs))] body)

dedictExpr _ _ = Nothing


---------------------------------------------------------------------
-- GENERAL INLINING

-- move Unknown's up the chain
inlineExpr :: ExprTweak
inlineExpr ihite (Call name []) | body == Unknown = Just Unknown
	where body = funcExpr $ getFunc ihite name
inlineExpr _ _ = Nothing
	
