
module Hite.Show() where

import Hite.Type
import List


instance Show Hite where
    show (Hite datas funcs) = unlines (map show datas ++ map show funcs)
    
instance Show Data where
    show (Data name ctors) = "data " ++ name ++ " = " ++ concat (intersperse " | " $ map show ctors)
    
instance Show Ctor where
    show (Ctor name args) = name ++ concatMap (' ':) args
    
instance Show Func where
    show (Func name args expr kind) =
        "\n" ++ name ++ concatMap (' ':) args ++
        " :: " ++ show kind ++ " = " ++ show expr

instance Show Kind where
    show Star = "*"
    show (Arrow xs) = concat $ intersperse "->" $ map f xs
        where
            f x@(Arrow _) = "(" ++ show x ++ ")"
            f x = show x
    
    show (Kinded as) = "{" ++ concat (intersperse ", " (map f as)) ++ "}"
        where
            f (name, arg) = concatMap (++ ".") name ++ show arg


instance Show Expr where
    show x = f False 0 x
        where
            brack True x = "(" ++ x ++ ")"
            brack False x = x
            
            f b i (Var name ""  ) = name
            f b i (Var name pare) = pare ++ "@" ++ name
            f b i (Bottom) = "_|_"
            f b i (RepeatNow) = "***"
            f b i (Repeat expr alt) = brack b $ f b i expr ++ " | " ++ f b i alt

            f b i (Sel expr arg) = f True i expr ++ "." ++ arg
            f b i (Make name args) = f b i (Call (CallFunc name) args)
            f b i (CallFunc name) = name
            f b i (Call (CallFunc name) []) = name
            f b i (Call name args) = brack b $ concat $ intersperse " " $
                                     map (f True i) (name:args)

            f b i (Case cond opts) = "case " ++ show cond ++ " of\n" ++
                                     if null opts then "    {- NO OPTIONS! -}" else
                                     (init $ unlines $ map (g (i+4)) opts)

            g i (a,b) = replicate i ' ' ++ a ++ " -> " ++ f False i b
            
            h i alt Nothing = "( *** | " ++ f True i alt ++ ")"
            h i alt (Just x) = f True i x
