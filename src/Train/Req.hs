
module Train.Req where

import Hite
import General.General
import Train.Path
import Data.BDD
import Data.List


-- module Train.Type
-- here to break cyclic dependancies

data ZExpr = ZCall FuncName [ZExpr]
		   | ZMake CtorName [ZExpr]
		   | ZSel ZExpr CtorArg
		   | ZVar FuncArg
		   | ZAny
		   deriving (Eq, Show, Ord)


instance Output ZExpr where
	output x = case x of
		ZCall x [] -> x
		ZCall x xs -> "(" ++ x ++ concatMap ((' ':) . output) xs ++ ")"
		ZMake x xs -> output (ZCall x xs)
		ZSel x y -> output x ++ "." ++ y
		ZVar x -> x
		ZAny -> "_"

-- end


-- DATA DEFINITIONS

type Scopes = BDD Scope
data Scope = Scope FuncName Reqs
			 deriving (Eq, Ord, Show)


type Reqs = BDD Req
data Req = Req Hite ZExpr Path [CtorName]
		   deriving (Show)

instance Eq Req where
	(Req _ a1 b1 c1) == (Req _ a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2

instance Ord Req where
	compare (Req _ a1 b1 c1) (Req _ a2 b2 c2) = compare (a1,b1,c1) (a2,b2,c2)

instance Output Scope where
	output (Scope name reqs) = "(\\forall " ++ name ++ ", " ++ output reqs ++ ")"

instance Output a => Output (BDD a) where
	output x = showBDDBy output x

instance Output Req where
	output (Req _ expr path ctor) =
		output expr ++ output path ++ strSet ctor


-- SMART CONSTRUCTORS



newReq :: Hite -> ZExpr -> Path -> [CtorName] -> Req
newReq hite zexpr path ctors
	| path == newPath hite ["tl"] && ctors == ["[]"] = Req hite zexpr (emptyPath hite) ctors
	| otherwise = Req hite zexpr path (nub $ sort ctors)

newReqs :: Hite -> ZExpr -> Path -> [CtorName] -> Reqs
newReqs hite zexpr path ctors | null ctors = bddFalse
							  | ctors `setEq` baseSet = bddTrue
							  | otherwise = bddLit $ newReq hite zexpr path ctors
	where
		baseSet = ctorNames $ getCtor hite (headNote "Train.Type.impliesReq" ctors)


-- UTILITIES


reqNot :: Req -> Req
reqNot (Req hite expr path ctors) = newReq hite expr path
	(ctorNames (getCtor hite (headNote "Train.Type.reqNot" ctors)) \\ ctors)

reqsNot :: Reqs -> Reqs
reqsNot x = bddNot reqNot x



-- SIMPLIFIERS

simplifyReqs = bddSimplify impliesReq

simplifyScopes = mapBDD f
	where
		f (Scope func xs) = if bddIsTrue res then bddTrue else bddLit $ Scope func res
			where res = simplifyReqs xs


impliesReq :: [(Req, Bool)] -> Req -> Maybe Bool
impliesReq given req@(Req hite on path ctors) = 
		if poss `subset` ctors then Just True
		else if ctors `disjoint` poss then Just False
		else Nothing
	where
		baseSet = ctorNames $ getCtor hite (headNote "Train.Type.impliesReq" ctors)
		poss = foldr f baseSet given
		
		f (Req _ on2 path2 ctors2, False) poss
			| on2 == on && path2 == path && finitePath path
			= poss \\ ctors2
		
		f (Req _ on2 path2 ctors2,True) poss
			| on2 == on && path `subsetPath` path2
			= poss `intersect` ctors2

		f _ poss = poss
		
impliesReq _ _ = Nothing



