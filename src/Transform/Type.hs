
module Transform.Type(module Transform.Type, module Hite.Type, module Hite.DataType) where

import Hite.Type(FuncName, FuncArg)
import Hite.DataType
import General.General
import Data.List


data IHite = IHite Datas [IFunc]

instance QDatas IHite where
	rawDatas (IHite res _) = res

instance Show IHite where
    show (IHite a b) = unlines $ concatMap (\x -> ["",show x]) b

data IFunc = Func {funcName :: FuncName, funcArgs :: [Int], funcExpr :: IExpr, funcTweaks :: [(Tweak, FuncName)]}
             deriving Eq

instance Show IFunc where
    show (Func name args body _) = name ++ concatMap ((' ':) . show) args ++ " =\n    " ++ show body

data IExpr = Var Int
		   | Make CtorName [IExpr]
           | Prim FuncName [IExpr]
		   | Call FuncName [IExpr]
		   | Case IExpr [(CtorName, IExpr)]
		   | Lambda [Int] IExpr
		   | Apply IExpr [IExpr]
		   | Sel IExpr CtorArg
		   | Error String
		   | Unknown

            -- used for Spec
           | Cell FuncPtr Int [IExpr] -- Partial call to a FuncPtr (num of args lacking)

		   deriving (Eq,Show)

data Tweak = Tweak String [String]
           | TweakExpr [IExpr] -- used for encoding FuncPtr's
			 deriving (Eq,Show)



data FuncPtr = FuncPtr FuncName [IExpr]
               deriving Eq


instance Show FuncPtr where
    show (FuncPtr name xs) = name ++ "{" ++ concat (intersperse "," $ map show2 xs) ++ "}"
        where
            show2 (Var 0) = "?"
            show2 (Make x xs) = show $ FuncPtr x xs
            show2 (Cell x n xs) = show x ++ ":" ++ show n
            show2 x = show x


-- the name of the introduce function should always be "" - blank

-- how you change a single expression
-- must be semantics preserving
type ExprTweak = IHite -> IExpr -> Maybe IExpr

-- how you change a function - the new body, the tweak, and the new function
type FuncTweak = IHite -> IFunc -> Maybe (IExpr, Tweak, IFunc)

-- how you change an expression that creates a new function
-- the new expression, the name of the func tweaked, the tweak, the new func
type FuncCreate = IHite -> IExpr -> Maybe (IExpr, FuncName, Tweak, IFunc)


getFunc :: IHite -> FuncName -> IFunc
getFunc ihite@(IHite _ funcs) name = case filter (\x -> funcName x == name) funcs of
	[] -> error $ "Could not find " ++ name ++ " in " ++ show (map funcName funcs) ++ ":\n" ++ output ihite
	[x] -> x
	xs -> error $ "Multiple defn of " ++ name ++ " in " ++ strSet (map funcName funcs) ++ ":\n" ++ output ihite



joinTweaks :: [a -> b -> Maybe c] -> a -> b -> Maybe c
joinTweaks [] a b = Nothing
joinTweaks (x:xs) a b =
	case x a b of
		Nothing -> joinTweaks xs a b
		res -> res




instance Manipulate IExpr where
    getChildren x = case x of
        Make _ xs -> xs
        Prim _ xs -> xs
        Call _ xs -> xs
        Cell _ _ xs -> xs
        Case x xs -> x : map snd xs
        Lambda _ x -> [x]
        Apply x xs -> x : xs
        Sel x _ -> [x]
        _ -> []

    setChildren x ys = case x of
        Make x _ -> Make x ys
        Call x _ -> Call x ys
        Cell x n _ -> Cell x n ys
        Prim x _ -> Prim x ys
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


freshFree :: IExpr -> [Int]
freshFree x = filter (`notElem` used) [0..]
	where
		used = nub $ concatMap f $ allIExpr x
		
		f (Var i) = [i]
		f (Lambda i _) = i
		f _ = []


collectFree :: IExpr -> [Int]
collectFree (Lambda x y) = collectFree y \\ x
collectFree (Var i) = [i]
collectFree x = nub $ concatMap collectFree $ getChildren x


replaceFree :: [(Int,IExpr)] -> IExpr -> IExpr
replaceFree rep x = case x of
	Var i -> lookupDefault (Var i) i rep
	Lambda i x -> Lambda i $ replaceFree [(a,b) | (a,b) <- rep, a `notElem` i] x
	x -> setChildren x (map (replaceFree rep) (getChildren x))


instance Output IHite where
	output (IHite _ xs) = concat $ intersperse "\n" $ map output xs


instance Output IFunc where
	output (Func name args body tweaks) =
		name ++ concatMap ((' ':) . show) args ++ " = " ++ t ++
		"\n" ++ indentStr (output body)
		where
			t = show tweaks


instance Output IExpr where
	output x = show x



validIHite :: IHite -> Bool
validIHite (IHite _ xs) = all validIFunc xs && length names == length (nub names)
	where names = map funcName xs

validIFunc :: IFunc -> Bool
validIFunc (Func name args body _) = sort args == sort (collectFree body)
