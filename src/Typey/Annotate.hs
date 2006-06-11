
module Typey.Annotate(annotate) where

import Hite
import Data.Predicate
import General.General
import Data.List
import Data.Char
import System.Cmd
import Typey.Type
import Typey.Read


annotate :: String -> Hite -> IO [(FuncName, FuncT)]
annotate file hite = do writeFile fileHs (toHaskell hite)
                        writeFile fileIn (getCommands hite)
                        system $ hugsPath ++ " " ++ fileHs ++ " < " ++ fileIn ++ " > " ++ fileOut
                        src <- readFile fileOut
                        return $ fixTypes $ parseTypes src
    where
        fileHs = "Cache/Example/catch_" ++ file ++ ".hs"
        fileIn = fileHs ++ ".in"
        fileOut = fileHs ++ ".out"

hugsPath = "\"C:/Program Files/WinHugs/hugs.exe\""


fixTypes :: [(FuncName, FuncT)] -> [(FuncName, FuncT)]
fixTypes x = [(decode a,mapLargeT f b) | (a,b) <- x]
    where
        f (CtorL name xs) = CtorL (decode name) xs
        f x = x



parseTypes :: String -> [(FuncName, FuncT)]
parseTypes xs = res
    where
        res = map getType $ filter ('=' `elem`) $ map fixType $ lines xs
        
        fixType (':':':':xs) = '=':xs
        fixType (x:xs) = x : fixType xs
        fixType xs = xs
        
        getType :: String -> (FuncName, FuncT)
        getType x = (init a,readFuncT $ tail b)
            where (a,_:b) = break (== '=') $ drop 2 $ dropWhile (/= '>') x


getCommands :: Hite -> String
getCommands hite = unlines $ map f (funcs hite)
    where
        f func = ":t " ++ enLower (funcName func)


prefix = 
    ["int_may f Nothing = False"
    ,"int_may f (Just x) = f x"
    ,"data " ++ box ++ " a = " ++ box ++ " | " ++ colon ++ " a (" ++ box ++ " a)"
    ]
    
    where
        colon = enUpper ":"
        box = enUpper "[]"


enUpper x = "CATCH_" ++ encode x
enLower x = "catch_" ++ encode x

encode :: String -> String
encode x = concatMap f x
    where
        f x | isAlphaNum x = [x]
            | otherwise = "_" ++ show (ord x) ++ "_"

decode x = f $ g $ dropWhile (/= '_') x
    where
        g (x:xs) = xs
        g _ = error $ "'" ++ x ++ "'"
    
        f ('_':xs) = chr (read a) : f (tail b)
            where (a,b) = break (== '_') xs
        f (x:xs) = x : f xs
        f [] = []


-- convert a Hite data structure, to a string
toHaskell :: Hite -> String
toHaskell (Hite datas funcs) = unlines $ prefix ++ concatMap fromData datas ++ concatMap fromFunc funcs
    where
    
    
        fromData :: Data -> [String]
        fromData (Data name ctors) = if name == "Char" then [] else concatMap f ctors
            where
                f (Ctor name args) = ["int_is_" ++ encode name ++ " (" ++ enUpper name ++ " {}) = True"
                                     ,"int_is_" ++ encode name ++ " _ = False"
                                     ] ++
                                     ["int_go_" ++ encode nam ++ lhs ++ show num | (num,nam) <- zip [1..] args]
                    where lhs = " (" ++ enUpper name ++ concat [" a" ++ show n | n <- [1..length args]] ++ ") = a"
        
        fromFunc (Func name args (MCase opts) _) =
            (enLower name ++ concatMap ((' ':) . enLower) args) : map fromOpt opts
        
        fromOpt :: MCaseAlt -> String
        fromOpt (MCaseAlt p x) = "   | " ++ fromCond p ++ " = " ++ fromExpr x
        
        fromCond :: Pred MCaseOpt -> String
        fromCond x | isTrue x = "True"
                   | otherwise = foldPred (g "or") (g "and") f x
            where
                f (MCaseOpt var ctor) = "(int_may int_is_" ++ encode ctor ++ " " ++ getVar True var ++ ")"
                g m xs = "(" ++ m ++ " [" ++ (concat $ intersperse "," xs) ++ "])"
              
        -- first param, should the thing be maybe wrapped
        getVar safe (Var x) = if safe then "(Just " ++ enLower x ++ ")" else enLower x
        getVar _ _ = "todo"

        fromExpr :: Expr -> String
        fromExpr (Call (CallFunc name) xs) = fromExprs (enLower name) xs
        fromExpr (Make name xs) = fromExprs (enUpper name) xs
        fromExpr (Var x) = enLower x
        fromExpr (Sel x y) = "(int_go_" ++ encode y ++ " " ++ fromExpr x ++ ")"
        fromExpr (Msg x) = show x

        fromExprs name xs = "(" ++ name ++ concatMap ((' ':) . fromExpr) xs ++ ")"