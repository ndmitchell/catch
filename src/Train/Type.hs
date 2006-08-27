
module Train.Type(module Train.Path, module Train.Type) where

import Data.BDD
import Data.List
import Data.Char
import Hite
import General.General
import Train.Path


data ZHite = ZHite [Data] [ZFunc]
			 deriving Show

data ZFunc = ZFunc FuncName [FuncArg] [(Reqs, Either String ZExpr)]
			 deriving Show

data ZExpr = ZCall FuncName [ZExpr]
		   | ZMake CtorName [ZExpr]
		   | ZSel ZExpr CtorArg
		   | ZVar FuncArg
		   deriving (Eq, Show, Ord)


instance Manipulate ZExpr where
	getChildren x = case x of
		ZCall _ x -> x
		ZMake _ x -> x
		ZSel x _ -> [x]
		ZVar _ -> []

	setChildren x ys = case x of
		ZCall x _ -> ZCall x ys
		ZMake x _ -> ZMake x ys
		ZSel _ x -> let [y] = ys in ZSel y x
		ZVar x -> ZVar x



type Scopes = BDD Scope
data Scope = Scope FuncName Reqs
			 deriving (Eq, Ord, Show)


type Reqs = BDD Req
data Req = Req Hite ZExpr Path [CtorName]
		   deriving (Show)

instance Eq Req where
	(Req _ a1 b1 c1) == (Req _ a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2

instance Ord Req where
	compare (Req _ a1 b1 c1) (Req _ a2 b2 c2) = compare a1 a2 `f` compare b1 b2 `f` compare c1 c2
		where
			f EQ a = a
			f x a = x


newReq :: Hite -> ZExpr -> Path -> [CtorName] -> Req
newReq hite zexpr path ctors
	| path == newPath ["tl"] && ctors == ["[]"] = Req hite zexpr emptyPath ctors
	| otherwise = Req hite zexpr path ctors


{-
instance PredLit Req where
	a ?=> b = a == b
	
	(Req _ a1 b1 c1) ?/\ (Req _ a2 b2 c2)
		| a1 == a2 && nullPath b1 && nullPath b2 && null (c1 `intersect` c2) = Value False
	_ ?/\ _ = Same


instance PredLit Scope where
	a ?=> b = a == b
	
	(Scope a1 b1) ?/\ (Scope a2 b2)
		| a1 == a2 = Single $ Scope a1 (predAnd [b1,b2])
		| otherwise = Same
	
	simp (Scope a b) | isTrue b = Just True
					 | otherwise = Nothing
-}

{-
instance BDDLit Req where
	litNot (Req hite expr path ctors) = bddLit $ Req hite expr path (getCtorsFromCtor hite (head ctors) \\ ctors)

instance BDDLit Scope where
	litNot (Scope name reqs) = bddLit $ Scope name (reqsNot reqs)
-}

reqNot :: Req -> Req
reqNot (Req hite expr path ctors) = newReq hite expr path (getCtorsFromCtor hite (head ctors) \\ ctors)

reqsNot :: Reqs -> Reqs
reqsNot x | bddIsTrue  x = bddFalse
		  | bddIsFalse x = bddTrue
		  | otherwise = mapBDD (bddLit . reqNot) x


instance Output ZExpr where
	output x = case x of
		ZCall x xs -> "(" ++ x ++ concatMap ((' ':) . output) xs ++ ")"
		ZMake x xs -> output (ZCall x xs)
		ZSel x y -> output x ++ "." ++ y
		ZVar x -> x


instance Output Scope where
	output (Scope name reqs) = "(\\forall " ++ name ++ ", " ++ output reqs ++ ")"

instance Output a => Output (BDD a) where
	output x = showBDDBy output x

instance Output Req where
	output (Req _ expr path ctor) =
		output expr ++ output path ++ strSet ctor
