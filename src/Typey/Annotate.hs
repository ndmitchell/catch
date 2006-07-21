
module Typey.Annotate(annotate, annotate2) where

import Hite
import Data.Predicate
import General.General
import Data.List
import Data.Char
import System.Cmd
import Typey.Type
import Typey.Read
import Typey.Read2


annotate :: String -> Hite -> IO [(FuncName, FuncT)]
annotate = annotateBase f
    where
        f = fixType . readFuncT

annotate2 :: String -> Hite -> IO [(FuncName, Large2T)]
annotate2 = annotateBase f
    where
        f = fixType2 . readFunc2T


annotateBase :: (String -> a) -> String -> Hite -> IO [(FuncName, a)]
annotateBase rd file hite = 
    do
        writeFile fileHs (toHaskell hite)
        writeFile fileIn (getCommands hite)
        system $ hugsPath ++ " " ++ fileHs ++ " < " ++ fileIn ++ " > " ++ fileOut
        src <- readFile fileOut
        checkForErrors src
        return [(a, rd b) | (a,b) <- parseTypes src]
    where
        fileHs = "Cache/Example/catch_" ++ file ++ ".hs"
        fileIn = fileHs ++ ".in"
        fileOut = fileHs ++ ".out"

hugsPath = "\"C:/Program Files/WinHugs/hugs.exe\""


checkForErrors :: String -> IO ()
checkForErrors xs = if null errlines then return ()
                    else do putStrLn "Errors when type checking:"
                            putStr $ unlines errlines
                            error "Failed with type check errors"
    where errlines = filter ("ERROR" `isPrefixOf`) (lines xs)



fixType :: FuncT -> FuncT
fixType x = mapLargeT f x
    where
        f (CtorL name xs) = CtorL (decode name) xs
        f x = x

fixType2 :: Large2T -> Large2T
fixType2 x = mapLarge2T f x
    where
        f (Ctor2T n) = Ctor2T (decode n)
        f x = x


parseTypes :: String -> [(FuncName, String)]
parseTypes xs = res
    where
        res = map getType $ filter ('=' `elem`) $ map fixType $ lines xs
        
        fixType (':':':':xs) = '=':xs
        fixType (x:xs) = x : fixType xs
        fixType xs = xs
        
        getType :: String -> (FuncName, String)
        getType x = (decode $ init a, tail b)
            where (a,_:b) = break (== '=') $ drop 2 $ dropWhile (/= '>') x


getCommands :: Hite -> String
getCommands hite = unlines $ map f (funcs hite)
    where
        f func = ":t " ++ enLower (funcName func)


prefix = 
        ["int_may f Nothing = False"
        ,"int_may f (Just x) = f x"
        ,"data " ++ box ++ " a = " ++ box ++ " | " ++ colon ++ " a (" ++ box ++ " a)"
        ,"data " ++ enUpper "Bool" ++ " = " ++ enUpper "True" ++ " | " ++ enUpper "False"
        ,"data " ++ enUpper "Compare" ++ " = " ++ intercat " | " (map enUpper ["EQ","LT","GT"])
        ] ++
        map makeTuple [1..8]
    where
        makeTuple n = "data " ++ tup ++ letters ++ " = " ++ tup ++ letters
            where
                tup = enUpper $ "Tup" ++ show n
                letters = ' ' : (intersperse ' ' $ take n ['a'..])

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
        g _ = error $ "While decoding '" ++ x ++ "'"
    
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
                                     ] ++ concat
                                     [
                                        ["int_go_" ++ encode nam ++ " " ++ lhs ++ " = a" ++ show num
                                        ,"int_gm_" ++ encode nam ++ " (Just " ++ lhs ++ ") = Just a" ++ show num
                                        ,"int_gm_" ++ encode nam ++ " _ = Nothing"]
                                        | (num,nam) <- zip [1..] args
                                     ]
                    where lhs = "(" ++ enUpper name ++ concat [" a" ++ show n | n <- [1..length args]] ++ ")"
        
        fromFunc (Func name args (MCase opts) _) =
            (enLower name ++ concatMap ((' ':) . enLower) args) : map fromOpt opts
        fromFunc (Func name args x _) =
            (enLower name ++ concatMap ((' ':) . enLower) args ++ " =") : ("  " ++ fromExpr x) : []
        
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
        getVar safe (Sel x y) | safe = "(int_gm_" ++ encode y ++ " " ++ getVar safe x ++ ")"
                              | otherwise = "(int_go_" ++ encode y ++ " " ++ getVar safe x ++ ")"

        fromExpr :: Expr -> String
        fromExpr (Call x xs) = fromExprs (fromExpr x) xs
        fromExpr (CallFunc x) = enLower x
        fromExpr (Make name xs) = fromExprs (enUpper name) xs
        fromExpr (Var x) = enLower x
        fromExpr (Sel x y) = "(int_go_" ++ encode y ++ " " ++ fromExpr x ++ ")"
        fromExpr (Msg x) = show x
        fromExpr (Error _) = "undefined"
        fromExpr (Case x alts) = "(case " ++ fromExpr x ++ " of" ++ concatMap f alts ++ ")"
            where
                f (ctor,expr) = " ; (" ++ enUpper ctor ++ "{}) -> " ++ fromExpr expr

        fromExprs name xs = "(" ++ name ++ concatMap ((' ':) . fromExpr) xs ++ ")"