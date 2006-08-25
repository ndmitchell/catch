
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
		   deriving (Eq, Show)


data Path = PathAtom CtorArg
	      | PathStar CtorArg
	      deriving (Eq, Show)


instance PredLit Req
instance PredLit Scope

instance PredLitNot Req where
	litNot (Req hite expr path ctors) = predLit $ Req hite expr path (getCtorsFromCtor hite (head ctors) \\ ctors)


instance Output ZExpr where
	output x = output $ f x
		where
			f (ZCall x xs) = Call (CallFunc x) (map f xs)
			f (ZMake x xs) = Make x (map f xs)
			f (ZSel x y) = Sel (f x) y
			f (ZVar x) = Var x

instance Output Scope where
	output (Scope name reqs) = "(\\forall " ++ name ++ ", " ++ output reqs ++ ")"

instance Output Req where
	output (Req _ expr path ctor) =
		output expr ++ concatMap (('.':) . output) path ++ strSet ctor

instance Output Path where
	output (PathAtom x) = x
	output (PathStar x) = x ++ "*"
