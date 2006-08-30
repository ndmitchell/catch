
module Transform.Type(module Transform.Type, module Hite.Type, module Hite.DataType) where

import Hite.Type(FuncName, FuncArg)
import Hite.DataType
import General.General
import Data.List


data IHite = IHite Datas [IFunc]

instance QDatas IHite where
	rawDatas (IHite res _) = res


data IFunc = Func {funcName :: FuncName, funcArgs :: [Int], funcExpr :: IExpr} deriving Show


data IExpr = Var Int
		   | Make CtorName [IExpr]
		   | Call FuncName [IExpr]
		   | Case IExpr [(CtorName, IExpr)]
		   | Lambda [Int] IExpr
		   | Apply IExpr [IExpr]
		   | Sel IExpr CtorArg
		   | Error String
		   | Unknown
		   deriving (Eq,Show)



type FuncTweak = IHite -> IFunc -> Maybe (IFunc, IExpr -> IExpr)

type ExprTweak = IHite -> IExpr -> Alteration

data Alteration = None
				| Change IExpr
				| Insert IFunc (FuncName -> IExpr)


getFunc :: IHite -> FuncName -> IFunc
getFunc (IHite _ funcs) name = head $ filter (\x -> funcName x == name) funcs


exprTweaks :: [ExprTweak] -> ExprTweak
exprTweaks [] ihite expr = None
exprTweaks (x:xs) ihite expr =
	case x ihite expr of
		None -> exprTweaks xs ihite expr
		x -> x

funcTweaks :: [FuncTweak] -> FuncTweak
funcTweaks [] ihite func = Nothing
funcTweaks (x:xs) ihite func =
	case x ihite func of
		Nothing -> funcTweaks xs ihite func
		x -> x




instance Manipulate IExpr where
	getChildren x = case x of
		Make _ xs -> xs
		Call _ xs -> xs
		Case x xs -> x : map snd xs
		Lambda _ x -> [x]
		Apply x xs -> x : xs
		Sel x _ -> [x]
		_ -> []

	setChildren x ys = case x of
		Make x _ -> Make x ys
		Call x _ -> Call x ys
		Case _ xs -> Case yh $ zip (map fst xs) yt
		Lambda x _ -> Lambda x y1
		Apply _ _ -> Apply yh yt
		Sel _ x -> Sel y1 x
		x -> x
		where
			(yh:yt) = ys
			[y1] = ys
	


allIExpr :: IExpr -> [IExpr]
allIExpr = allOver

mapIExpr :: (IExpr -> IExpr) -> IExpr -> IExpr
mapIExpr = mapOver


freshFree :: [IExpr] -> [Int]
freshFree xs = filter (`notElem` used) [0..]
	where used = nub [i | x <- xs, Var i <- allIExpr x]


instance Output IHite where
	output (IHite _ xs) = concat $ intersperse "\n" $ map output xs


instance Output IFunc where
	output (Func name args body) = name ++ concatMap ((' ':) . show) args ++ " = \n" ++ indentStr (output body)


instance Output IExpr where
	output x = show x
