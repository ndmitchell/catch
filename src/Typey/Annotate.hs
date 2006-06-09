
module Typey.Annotate(annotate) where

import Hite
import Data.Predicate
import General.General
import Data.List


annotate :: String -> Hite -> IO Hite
annotate file hite = do writeFile file2 (toHaskell hite)
                        return hite
    where
        file2 = "Cache/Example/catch_" ++ file ++ ".hs"


prefix = 
    ["catch_may f Nothing = False"
    ,"catch_may f (Just x) = f x"
    ]


encode :: String -> String
encode x = x


-- convert a Hite data structure, to a string
toHaskell :: Hite -> String
toHaskell (Hite datas funcs) = unlines $ prefix ++ concatMap fromData datas ++ concatMap fromFunc funcs
    where
    
    
        fromData :: Data -> [String]
        fromData (Data name ctors) = if name == "Char" then [] else concatMap f ctors
            where
                f (Ctor name args) = ["catch_is_" ++ name ++ " (" ++ name ++ " {}) = True"
                                     ,"catch_is_" ++ name ++ " _ = False"
                                     ,"catch_ism_" ++ name ++ " = catch_may catch_is_" ++ name
                                     ]
        
        fromFunc (Func name args (MCase opts) _)
            | name == "error" = []
            | otherwise = (name ++ concatMap (' ':) args) : map fromOpt opts
        
        fromOpt :: MCaseAlt -> String
        fromOpt (MCaseAlt p x) = "   | " ++ fromCond p ++ " = " ++ fromExpr x
        
        fromCond :: Pred MCaseOpt -> String
        fromCond x | isTrue x = "True"
                   | otherwise = foldPred (g "or") (g "and") f x
            where
                f (MCaseOpt var ctor) = "(catch_ism_" ++ encode ctor ++ " " ++ getVar True var ++ ")"
                g m xs = "(" ++ m ++ " [" ++ (concat $ intersperse "," xs) ++ "])"
              
        -- first param, should the thing be maybe wrapped
        getVar safe (Var x) = if safe then "(Just " ++ encode x ++ ")" else encode x
        getVar _ _ = "todo"

        fromExpr :: Expr -> String
        fromExpr x = output x
