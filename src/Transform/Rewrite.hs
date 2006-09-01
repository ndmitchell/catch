
module Transform.Rewrite(exprTweak, funcTweak, funcCreate) where

import Transform.Type

import Control.Exception
import Data.List
import General.General


exprTweak = joinTweaks [basicExpr, inlineExpr]

funcTweak = joinTweaks [deadArg, lambdaRaise]

funcCreate = joinTweaks [defuncExpr, dedictExpr]


---------------------------------------------------------------------
-- ATOM TESTS

isTuple (Make ('T':'u':'p':_) _) = True
isTuple _ = False

isVar (Var _) = True
isVar _ = False


---------------------------------------------------------------------
-- INLINE RULES

-- a lot of things will require inline stuff

inlineExpr :: ExprTweak
inlineExpr ihite (Call name params) | canInline name body
		= Just (replaceFree (zip args params) body)
	where Func _ args body _ = getFunc ihite name
inlineExpr _ _ = Nothing


canInline :: String -> IExpr -> Bool
canInline name Unknown = True
canInline name x | isTuple x = True
canInline name (Call nam xs) | nam /= name && all isVar xs = True
canInline name (Lambda _ x) | canInline name x = True
canInline name _ = False


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
basicExpr ihite (Case (Make name args) xs) = Just $ headNote "Tranform.Rewrite.basicExpr" ys
	where ys = [b | (a,b) <- xs, a == name]

-- Apply (Lambda free expr) args  ==> expr[free/args]
basicExpr ihite (Apply (Lambda (f:ree) expr) (a:rgs)) =
	Just $ Apply (Lambda ree (replaceFree [(f,a)] expr)) rgs

basicExpr ihite _ = Nothing



---------------------------------------------------------------------
-- DEAD ARGUMENT ELIMINATION

-- f a b = expr_on_b  ==>  f b = expr_on_b
deadArg :: FuncTweak
deadArg ihite (Func name args body _)
		| name /= "main" && not (null deads) && not (canInline name body)
		= Just (newexpr, Tweak "deadArg" (map show deads), Func "" alives body [])
	where
		alives = collectFree body
		deads = args \\ alives
		newexpr = Call "" (map Var alives)

deadArg ihite _ = Nothing


---------------------------------------------------------------------
-- LAMBDA RAISE

-- f a = Lambda n b ==> f a n = b
lambdaRaise :: FuncTweak
lambdaRaise ihite (Func name args lam@(Lambda xs body) _)
		| not $ canInline name lam
		= Just (Lambda xs $ Call "" (map Var (args++xs)),
				Tweak "lambdaRaise" [show $ length xs],
				Func "" (args++xs) body [])

lambdaRaise _ _ = Nothing


---------------------------------------------------------------------
-- DEFUNCTIONALISE FUNCTION

-- f (pre:Lambda args (Call x xs):pre)  ==>
--     f' pre:fv:post,  f'=f[arg/Lambda args (Call x xs)]
defuncExpr :: FuncCreate
defuncExpr ihite (Call name xs) | any isHO xs
		= Just (newcall,name,tweak,newfunc)
	where
		(pre,spec:post) = break isHO xs
		fvSpec = collectFree spec
		
		newcall = Call "" (pre ++ map Var fvSpec ++ post)
		tweak = Tweak "defuncExpr" [show $ length pre, show spec] -- too specific, can be loosend
		
		Func _ args body _ = getFunc ihite name
		newargs = take (length fvSpec) $ freshFree body \\ args
		
		lpre = length pre
		newfunc = Func ""
			(take lpre args ++ newargs ++ drop (lpre+1) args)
			(replaceFree [(args!!lpre, replaceFree (zip fvSpec (map Var newargs)) spec)] body)
			[]
		
		-- UNSURE IF THIS IS CORRECT
		-- WHAT IF ARBITRARY EXPRESSION, LEADS TO NON-TERM?
		-- Potentially unsafe generalisation
		-- isHO (Lambda _ _) = True

		{-
		isHO (Lambda xs (Call _ args)) |
				fake == map Var xs &&
				(nub (concatMap collectFree real) `disjoint` xs) = True
			where (real,fake) = splitAt (length args - length xs) args
		-}
		isHO (Lambda _ (Call _ _)) = True
		isHO (Lambda _ Unknown) = True
		isHO _ = False

defuncExpr _ _ = Nothing



---------------------------------------------------------------------
-- DICTIONARY REMOVAL

-- remove dictionaries, see MonadFail2 for an example

dedictExpr :: FuncCreate
dedictExpr ihite (Call name xs) | any isTuple xs
		= Just (newcall,name,tweak,newfunc)
	where
		(pre,Make tupn tupArgs:post) = break isTuple xs
		tweak = Tweak "dedictExpr" [show lpre,show $ length tupArgs]
		
		newcall = Call "" (pre ++ tupArgs ++ post)
		
		Func _ args body _ = getFunc ihite name
		newargs = take (length tupArgs) $ freshFree body \\ args

		lpre = length pre
		newfunc = Func name
			(take lpre args ++ newargs ++ drop (lpre+1) args)
			(replaceFree [(args!!lpre,Make tupn (map Var newargs))] body)
			[]

dedictExpr _ _ = Nothing
