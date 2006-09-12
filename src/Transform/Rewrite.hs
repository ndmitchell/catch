
module Transform.Rewrite(exprTweak, funcTweak, funcCreate) where

import Transform.Type

import Control.Exception
import Data.List
import Data.Maybe
import General.General


exprTweak = joinTweaks [basicExpr, inlineExpr]

funcTweak = joinTweaks [deadArg, lambdaRaise]

funcCreate = joinTweaks [defuncExpr, dedictExpr]


---------------------------------------------------------------------
-- ATOM TESTS

isTuple (Make ('T':'u':'p':_) _) = True
isTuple (Make ('(':_) _) = True
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
canInline name x | isVar x = True
canInline name _ = False


---------------------------------------------------------------------
-- BASIC RULES


basicExpr :: ExprTweak

-- structural simplifications
basicExpr ihite (Apply x []) = Just x
basicExpr ihite (Lambda [] x) = Just x
basicExpr ihite (Call "_" []) = Just Unknown

-- (Cons a b).head ==> a
-- Nil.head ==> (no change) - may be in a redundant branch
basicExpr ihite orig@(Sel (Make name args) arg)
        | ctorName x == name
        = Just $ args !! cargPos x
	where x = getCArg ihite arg

-- case Nil of {Nil -> a; Cons -> b} ==> a
basicExpr ihite (Case (Make name args) xs) = Just $ headNote "Tranform.Rewrite.basicExpr" ys
	where ys = [b | (a,b) <- xs, a == name || null a]

-- Apply (Lambda free expr) args  ==> expr[free/args]
basicExpr ihite (Apply (Lambda (f:ree) expr) (a:rgs)) =
	Just $ Apply (Lambda ree (replaceFree [(f,a)] expr)) rgs

-- not so basic, Case x [(_, Lambda (x:_) _), ...]
-- lift a lambda out of a Case
basicExpr ihite expr@(Case x xs) | not (null xs) && all (isLamOne . snd) xs =
	Just $ Lambda [free] $ Case x [(a, Lambda c (replaceFree [(b,Var free)] d)) | (a,Lambda (b:c) d) <- xs]
	where
		free = head $ freshFree expr
		isLamOne (Lambda (_:_) _) = True
		isLamOne _ = False

-- Lambda x (Lambda y z) ==> Lambda (x++y) z
basicExpr ihite (Lambda x (Lambda y z)) | x `disjoint` y = Just $ Lambda (x++y) z

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
defuncExpr ihite (Call name xs) | any (isJust . testHO) xs
		= Just (newcall,name,tweak,newfunc)
	where
		(pre,rep:post) = break (isJust . testHO) xs
		Just (addArgs,twk,caller,bod) = testHO rep
		
		newcall = Call "" (pre ++ caller ++ post)
		tweak = Tweak "defuncExpr" (show (length pre) : twk)
		
		Func _ args body _ = getFunc ihite name
		newargs = freshFree body \\ args
		
		lpre = length pre
		newfunc = Func ""
			(take lpre args ++ addArgs ++ drop (lpre+1) args)
			(replaceFree [(args!!lpre, bod)] body)
			[]
		
		
		testHO :: IExpr -> Maybe ([Int],[String],[IExpr],IExpr)
		
		-- defunc those which have no free variables
		testHO (Lambda xs x) | collectFree x `disjoint` xs =
			Just ([thisarg], [show lxs], [x], Lambda lfree (Var thisarg))
			where
				lfree = take lxs $ filter (/= thisarg) [1..]
				lxs = length xs
				thisarg = head newargs
		
		-- defunc function calls, where all additional vars are passed straight through
		testHO (Lambda args (Call nam xs))
				| concatMap collectFree real `disjoint` args && fake == map Var args
				= Just (thisargs,[nam,show $ length real],real,
				 	    Lambda lfree (Call nam (map Var (thisargs++lfree))))
			where
				lfree = take (length args) $ filter (`notElem` thisargs) [1..]
				thisargs = take (length real) newargs
				(real,fake) = splitAt (length xs - length args) xs

		-- defunc function calls, where the parameters are at the front (getting hacky..)
		testHO (Lambda args (Call nam xs))
				| concatMap collectFree real `disjoint` args && fake == map Var args
				= Just (thisargs,[nam,"R",show $ length real],real,
						Lambda lfree (Call nam (map Var (lfree++thisargs))))
			where
				lfree = take (length args) $ filter (`notElem` thisargs) [1..]
				thisargs = take (length real) newargs
				(fake,real) = splitAt (length args) xs

		
		-- an icky special case for id
		testHO (Lambda [i] (Var j)) | i == j
			= Just ([],["-ID-"],[],Lambda [i] (Var i))
		
		-- those which are entirely free, no external context
		testHO expr@(Lambda args body) | null (collectFree expr)
				= Just ([],[show $ expr2],[],expr2)
			where
				expr2 = Lambda args2 $ replaceFree (zip args (map Var args2)) body
				args2 = [1..length args]
		
		testHO _ = Nothing

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
