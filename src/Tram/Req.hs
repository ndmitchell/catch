
module Tram.Req where

import Hill.All
import General.General
import Tram.Path
import Data.Proposition
import Data.List
import Safe


-- DATA DEFINITIONS

type Scopes = [Scope]
data Scope = Scope FuncName Reqs
			 deriving (Eq, Ord)

type Reqs = BDD Req
data Req = Req Hill Expr Path [CtorName]

instance Eq Req where
	(Req _ a1 b1 c1) == (Req _ a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2

instance Ord Req where
	compare (Req _ a1 b1 c1) (Req _ a2 b2 c2) = compare (a1,b1,c1) (a2,b2,c2)

instance Show Scope where
	show (Scope name reqs) = "(\\forall " ++ name ++ ", " ++ show reqs ++ ")"

instance Show Req where
	show (Req _ expr path ctor) =
		show expr ++ show path ++ strSet ctor


-- SMART CONSTRUCTORS

scopesAnds :: Scopes -> Scopes
scopesAnds xs = filter (\(Scope a b) -> not (propIsTrue b)) $ map f $
               groupSetExtract (\(Scope a b) -> a) xs
    where
        f xs@(Scope a _:_) = Scope a $ propAnds [b | Scope a b <- xs]




newReq :: Hill -> Expr -> Path -> [CtorName] -> Req
newReq hite zexpr path ctors
--	| path == newPath hite ["tl"] && ctors == ["[]"] = Req hite zexpr (emptyPath hite) ctors
	| otherwise = Req hite zexpr path (nub $ sort ctors)

newReqs :: Hill -> Expr -> Path -> [CtorName] -> Reqs
newReqs hite zexpr path ctors | null ctors = propFalse
							  | ctors `setEq` baseSet = propTrue
							  | otherwise = propLit $ newReq hite zexpr path ctors
	where
		baseSet = ctorNames $ getCtor hite (headNote "Tram.Type.impliesReq here" ctors)


-- UTILITIES

instance PropLit Req where

    (?=>) = impliesReq
    (?/\) a b = case combineReqsAnd a b of
                    Nothing -> None
                    Just x -> Value x


instance PropNot Req where
	litNot (Req hite expr path ctors) =
		newReq hite expr path
		(ctorNames (getCtor hite (headNote "Tram.Type.reqNot" ctors)) \\ ctors)

reqsNot :: Reqs -> Reqs
reqsNot x = propNot x

-- instance PropLit Scope where
	


-- SIMPLIFIERS

simplifyReqs x = propSimplify x -- bddSimplify impliesReq . bddApplyAnd combineReqsAnd

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
		baseSet = ctorNames $ getCtor hite (headNote "Tram.Type.impliesReq" ctors)
		poss = foldr f baseSet given
		
		f (Req _ on2 path2 ctors2, False) poss
			| on2 == on && path2 == path && finitePath path
			= poss \\ ctors2
		
		f (Req _ on2 path2 ctors2,True) poss
			| on2 == on && path `subsetPath` path2
			= poss `intersect` ctors2

		f _ poss = poss
		
impliesReq _ _ = Nothing



