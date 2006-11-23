
{-! global: GhcBinary !-}

module Hill.Type(module Hill.Type, module Hite.DataType, module Hite.TypeType) where

import Hite.DataType
import Hite.TypeType
import Front.CmdLine
import Control.Monad
import General.General
import General.Play
import Data.List
import Data.Char
import Control.Exception
import Yhc.Core


data Hill = Hill {datas :: [Data], funcs :: [Func]}

instance QDatas Hill where
    rawDatas = datas


data Func = Func {funcName :: FuncName, funcArgs :: [Int], body :: Expr}


getFunc :: Hill -> FuncName -> Func
getFunc hill name = case [x | x <- funcs hill, funcName x == name] of
    [] -> error $ "Hill.getFunc, can't find " ++ name
    [x] -> x
    _ -> error $ "Hill.getFunc, found multiple " ++ name


mkLet [] x = x
mkLet xs x = Let xs x

mkApply x [] = x
mkApply (Apply x xs) ys = mkApply x (xs ++ ys)
mkApply (Case on alts) ys = Case on [alt{altExpr = mkApply (altExpr alt) ys} | alt <- alts]
mkApply x xs = Apply x xs

mkLambda 0 x = x
mkLambda n x = Lambda n x

dropLambda (Lambda n x) = x
dropLambda x = x

dropLet (Let _ x) = dropLet x
dropLet x = x

fromLet (Let x y) = (x,y)
fromLet x = ([],x)

fromVar (Var x) = x

fromFun (Fun x) = x

elimLet x = mapOverHill dropLet x


isVar (Var _) = True; isVar _ = False

isFun (Fun _) = True; isFun _ = False

isVarSel (Sel x _) = isVarSel x
isVarSel x = isVar x

isMake (Make{}) = True; isMake _ = False

isConst (Const _) = True; isConst _ = False

isError (Error{}) = True; isError _ = False

isDefault (Default{}) = True; isDefault _ = False

expandStr1 :: String -> Expr
expandStr1 "" = Make "[]" []
expandStr1 (x:xs) = Make ":" [Const (AChar x), Const (AString xs)]


expandStr :: String -> Expr
expandStr "" = Make "[]" []
expandStr (x:xs) = Make ":" [Const (AChar x), expandStr xs]


filterAltsConst :: Const -> [Alt] -> [Alt]
filterAltsConst c alts = filter f alts
    where
        f (Default _) = True
        f (AltConst x _) = x == c
        f _ = False

filterAltsCtr :: CtorName -> [Alt] -> [Alt]
filterAltsCtr c alts = filter f alts
    where
        f (Default _) = True
        f (AltCtr x _) = x == c
        f _ = False


data Expr = 
            -- atoms
            Var Int
          | Star
          | Fun FuncName
          | Ctr CtorName
          | Const Const
            
            -- calls
          | Call FuncName [Expr]
          | Make CtorName [Expr]
          | Prim String   [Expr]
          
            -- structure
          | Sel Expr CtorArg
          | Let [(Int, Expr)] Expr
          | Case Expr [Alt]
          -- | MCase ...
          
            -- higher order bits
          | Lambda Int Expr
          | Apply Expr [Expr]
          
            -- other stuff
          | Error Expr
          deriving (Eq, Ord)


data Const = AInt Int
           | AInteger Integer
           | AFloat Float
           | ADouble Double
           | AChar Char
           | AString String
          deriving (Eq, Ord)


data Alt = Default {altExpr :: Expr}
         | AltConst {altVal :: Const, altExpr :: Expr}
         | AltCtr {altCtr :: CtorName, altExpr :: Expr}
         deriving (Eq, Ord)


-- Manipulate stuff


instance Play Expr where
    replaceChildren x = case x of
        Call x xs -> (xs, Call x)
        Make x xs -> (xs, Make x)
        Prim x xs -> (xs, Prim x)
          
        Sel x y -> playOne (`Sel` y) x
        Let xs x -> (x:xr, \(y:ys) -> Let (zip xl ys) y)
            where (xl,xr) = unzip xs
        Case x y -> (x:map altExpr y, \(x2:y2) -> Case x2 (zipWith (\a b -> a{altExpr=b}) y y2))

        Lambda n x -> playOne (Lambda n) x
        Apply x xs -> (x:xs, \(x:xs) -> Apply x xs)
        
        Error x -> playOne Error x
        x -> playDefault x


instance PlayEx Func Expr where
    replaceChildrenEx func = ([body func], \[x] -> func{body=x})

instance PlayEx Hill Expr where
    replaceChildrenEx hill =
        (
            bods,
            \xs -> hill{funcs = zipWith (\func bod -> func{body=bod}) (funcs hill) bods}
        )
        where bods = map body (funcs hill)



instance Manipulate Expr where
    getChildren x = case x of
        Call _ xs -> xs
        Make _ xs -> xs
        Prim _ xs -> xs
          
        Sel x _ -> [x]
        Let xs x -> x : map snd xs
        Case x y -> x : map altExpr y

        Lambda _ x -> [x]
        Apply x xs -> x:xs
        
        Error x -> [x]
        _ -> []
    
    setChildren x xs = case x of
        Call y _ -> Call y xs
        Make y _ -> Make y xs
        Prim y _ -> Prim y xs
        
        Sel _ y -> Sel xs1 y
        Let ys _ -> Let (zip (map fst ys) xst) xsh
        Case _ ys -> Case xsh (zipWith f ys xst)
            where f y x = y{altExpr = x}
        
        Lambda y _ -> Lambda y xs1
        Apply _ _ -> Apply xsh xst
        
        Error _ -> Error xs1
        x -> assert (null xs) x
        where
            [xs1] = xs
            (xsh:xst) = xs


class ManipulateHill a where
    mapOverHill :: (Expr -> Expr) -> a -> a
    allOverHill :: a -> [Expr]


instance ManipulateHill Hill where
    mapOverHill f (Hill a b) = Hill a (mapOverHill f b)
    allOverHill (Hill a b) = allOverHill b

instance ManipulateHill Func where
    mapOverHill f func = func{body = mapOverHill f (body func)}
    allOverHill func = allOverHill (body func)

instance ManipulateHill a => ManipulateHill [a] where
    mapOverHill f xs = map (mapOverHill f) xs
    allOverHill xs = concatMap allOverHill xs

instance ManipulateHill Expr where
    mapOverHill f xs = mapOver f xs
    allOverHill xs = allOver xs



-- command stuff

data Value = ValueHill {valueHill :: Hill}
           | ValueFile {valueFile :: FilePath}
           | ValueCore {valueCore :: Core}
           | ValueBool {valueBool :: Bool}

type HillAction = Action Value


hillCmd :: String -> (String -> Hill -> IO Hill) -> HillAction
hillCmd a b = Action ("hill-" ++ a) (\state extra x -> liftM ValueHill $ b extra $ valueHill x)

hillCmdPure :: String -> (String -> Hill -> Hill) -> HillAction
hillCmdPure a b = hillCmd a (\x y -> return $ b x y)


-- more complex manipulations

replaceFree :: [(Int, Expr)] -> Expr -> Expr
replaceFree ren (Var x) = case lookup x ren of
                              Nothing -> Var x
                              Just y -> y

replaceFree ren (Let binds x) = Let binds2 $ replaceFree (filter isValid ren) x
    where
        isValid (i,_) = not $ i `elem` map fst binds
        binds2 = [(a, replaceFree ren b) | (a,b) <- binds]

replaceFree ren x = setChildren x $ map (replaceFree ren) $ getChildren x


requiredFree :: Expr -> [Int]
requiredFree x = snub $ f x
    where
        f (Let binds x) = (f x \\ map fst binds) ++ (concatMap (f . snd) binds)
        f (Var x) = [x]
        f x = concatMap f (getChildren x)


usedFree :: Expr -> [Int]
usedFree x = snub $ concatMap f $ allOverHill x
    where
        f (Let binds x) = map fst binds
        f (Var x) = [x]
        f _ = []


freshFree :: Expr -> [Int]
freshFree x = [0..] \\ usedFree x


freshFreeFunc :: Func -> [Int]
freshFreeFunc x = freshFree (body x) \\ funcArgs x


-- unique numbers and names

calcUnique :: Hill -> Int
calcUnique hill = 1 + maximum (0 : concatMap (f . funcName) (funcs hill))
    where
        f x = if not (null b) && all isDigit a then [read $ reverse a] else []
            where (a,b) = break (== '_') $ reverse x


genUnique :: FuncName -> Int -> FuncName
genUnique name i = name ++ "_" ++ show i
