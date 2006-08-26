
module Train.Type where

import Data.Predicate
import Data.List
import Hite
import General.General


data ZHite = ZHite [Data] [ZFunc]
			 deriving Show

data ZFunc = ZFunc FuncName [FuncArg] [(Reqs, Either String ZExpr)]
			 deriving Show

data ZExpr = ZCall FuncName [ZExpr]
		   | ZMake CtorName [ZExpr]
		   | ZSel ZExpr CtorArg
		   | ZVar FuncArg
		   deriving (Eq, Show)


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



type Scopes = Pred Scope
data Scope = Scope FuncName Reqs
			 deriving (Eq, Show)


type Reqs = Pred Req
data Req = Req Hite ZExpr [Path] [CtorName]
		   deriving (Show)

instance Eq Req where
	(Req _ a1 b1 c1) == (Req _ a2 b2 c2) = a1 == a2 && b1 == b2 && c1 == c2


data Path = PathAtom CtorArg
	      | PathStar CtorArg
	      deriving (Eq, Show)


isStar x = x `elem` ["tl"]

integrate :: [Path] -> CtorArg -> [Path]
integrate ys@(PathStar y:_) x | x == y = ys
integrate ys x = (if isStar x then PathStar x else PathAtom x) : ys


instance PredLit Req where
	a ?=> b = a == b

instance PredLit Scope where
	a ?=> b = a == b
	
	(Scope a1 b1) ?/\ (Scope a2 b2)
		| a1 == a2 = Single $ Scope a1 (predAnd [b1,b2])
		| otherwise = Same
	
	simp (Scope a b) | isTrue b = Just True
					 | otherwise = Nothing

instance PredLitNot Req where
	litNot (Req hite expr path ctors) = predLit $ Req hite expr path (getCtorsFromCtor hite (head ctors) \\ ctors)


instance Output ZExpr where
	output x = case x of
		ZCall x xs -> "(" ++ x ++ concatMap ((' ':) . output) xs ++ ")"
		ZMake x xs -> output (ZCall x xs)
		ZSel x y -> output x ++ "." ++ y
		ZVar x -> x


instance Output Scope where
	output (Scope name reqs) = "(\\forall " ++ name ++ ", " ++ output reqs ++ ")"

instance Output Req where
	output (Req _ expr path ctor) =
		output expr ++ concatMap (('.':) . output) path ++ strSet ctor

instance Output Path where
	output (PathAtom x) = x
	output (PathStar x) = x ++ "*"
