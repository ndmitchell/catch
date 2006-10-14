
{-! global: GhcBinary !-}

module Hill.Type(module Hill.Type, module Hite.DataType, module Hite.TypeType) where

import Hite.DataType
import Hite.TypeType
import Front.CmdLine
import Control.Monad
import General.General
import Data.List
import Control.Exception


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


isVar (Var _) = True; isVar _ = False


data Expr = 
            -- atoms
            Var Int
          | Fun FuncName
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
           | ACtor CtorName
          deriving (Eq, Ord)


data Alt = Default {altExpr :: Expr}
         | Alt {altVal :: Const, altExpr :: Expr}
         deriving (Eq, Ord)


-- Manipulate stuff

instance Manipulate Expr where
    getChildren x = case x of
        Call _ xs -> xs
        Make _ xs -> xs
        Prim _ xs -> xs
          
        Sel x _ -> [x]
        Let xs x -> x : map snd xs
        Case x y -> x : map f y
            where
                f (Default x) = x
                f (Alt _ x) = x

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
            where
                f (Default _) x = Default x
                f (Alt y _) x = Alt y x
        
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

data ValueHill = ValueHill Hill
               | ValueNone

type HillAction = Action ValueHill


hillCmd :: String -> (String -> Hill -> IO Hill) -> HillAction
hillCmd a b = Action ("hill-" ++ a) (\state extra (ValueHill x) -> liftM ValueHill $ b extra x)

hillCmdPure :: String -> (String -> Hill -> Hill) -> HillAction
hillCmdPure a b = hillCmd a (\x y -> return $ b x y)


-- more complex manipulations

replaceFree :: [(Int, Expr)] -> Expr -> Expr
replaceFree ren (Var x) = case lookup x ren of
                              Nothing -> Var x
                              Just y -> y

replaceFree ren (Let binds x) = Let binds $ replaceFree (filter isValid ren) x
    where isValid (i,_) = not $ i `elem` map fst binds

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
