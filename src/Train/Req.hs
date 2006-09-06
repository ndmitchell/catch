
module Train.Req where

import Hite
import General.General
import Train.Path
import Data.Proposition
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

type Scopes = [Scope]
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
	output x = propShowBy output x

instance Output Req where
	output (Req _ expr path ctor) =
		output expr ++ output path ++ strSet ctor


-- SMART CONSTRUCTORS

scopesAnds :: Scopes -> Scopes
scopesAnds xs = filter (\(Scope a b) -> not (propIsTrue b)) $ map f $
               groupSetExtract (\(Scope a b) -> a) xs
    where
        f xs@(Scope a _:_) = Scope a $ propAnds [b | Scope a b <- xs]




newReq :: Hite -> ZExpr -> Path -> [CtorName] -> Req
newReq hite zexpr path ctors
--	| path == newPath hite ["tl"] && ctors == ["[]"] = Req hite zexpr (emptyPath hite) ctors
	| otherwise = Req hite zexpr path (nub $ sort ctors)

newReqs :: Hite -> ZExpr -> Path -> [CtorName] -> Reqs
newReqs hite zexpr path ctors | null ctors = propFalse
							  | ctors `setEq` baseSet = propTrue
							  | otherwise = propLit $ newReq hite zexpr path ctors
	where
		baseSet = ctorNames $ getCtor hite (headNote "Train.Type.impliesReq here" ctors)


-- UTILITIES

instance PropLit Req where

instance PropNot Req where
	litNot (Req hite expr path ctors) =
		newReq hite expr path
		(ctorNames (getCtor hite (headNote "Train.Type.reqNot" ctors)) \\ ctors)

reqsNot :: Reqs -> Reqs
reqsNot x = propNot x

-- instance PropLit Scope where
	


-- SIMPLIFIERS

simplifyReqs = bddSimplify impliesReq . bddApplyAnd combineReqsAnd

--simplifyScopes = id -- mapBDD f . bddApplyAnd combineScopesAnd
--	where
--		f (Scope func xs) = newScopes func (simplifyReqs xs)


combineReqsAnd :: Req -> Req -> Maybe Req
combineReqsAnd (Req hite on1 path1 ctors1) (Req _ on2 path2 ctors2)
	| on1 == on2 && path1 == path2 = Just (Req hite on1 path1 (sort $ ctors2 `intersect` ctors1))
	| otherwise = Nothing


--combineScopesAnd :: Scope -> Scope -> Maybe Scope		
--combineScopesAnd (Scope f1 x1) (Scope f2 x2)
--	| f1 == f2 = Just (Scope f1 (propAnd x1 x2))
--	| otherwise = Nothing


impliesReq :: [(Req, Bool)] -> Req -> Maybe Bool
impliesReq given req@(Req hite on path ctors) = 
        if null ctors then Just False
		else if poss `subset` ctors then Just True
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



