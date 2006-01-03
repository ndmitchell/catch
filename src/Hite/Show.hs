
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
    show (Func name args expr) = "\n" ++ name ++ concatMap (' ':) args ++ " = " ++ show expr
    
instance Show Expr where
    show x = f False 0 x
        where
            brack True x = "(" ++ x ++ ")"
            brack False x = x
            
            f b i (Var name pare) = name
            f b i (Sel expr arg) = f True i expr ++ "." ++ arg
            f b i (Make name args) = f b i (Call (CallFunc name) args)
            f b i (CallFunc name) = name
            f b i (Call (CallFunc name) []) = name
            f b i (Call name args) = brack b $ concat $ intersperse " " $
                                     map (f True i) (name:args)

            f b i (Case cond opts) = "case " ++ show cond ++ " of\n" ++
                                     (init $ unlines $ map (g (i+4)) opts)

            g i (a,b) = replicate i ' ' ++ a ++ " -> " ++ f False i b

{-
showData x = "data = " ++ concat (intersperse " | " x)

showFunc (name, expr) = "\n" ++ name ++ " = " ++ showExpr id expr


brack x = "(" ++ x ++ ")"


showExpr b (Var name sels) = "#" ++ show name ++ concatMap (\x -> '.':show x) sels

showExpr b (Call name args) = showExpr b (Make (showExpr brack name) args)

showExpr b (Make name args) =
    (if null args then id else b) $
    name ++ concatMap (\x -> ' ': showExpr brack x) args
    
showExpr b (Case expr alts) =
    "case " ++ showExpr brack expr ++ " of" ++
    indent (concatMap (\(opt, val) -> "\n" ++ opt ++ " -> " ++ showExpr id val) alts)

showExpr b (CallFunc x) = x
    
indent [] = []
indent ('\n':xs) = '\n':'\t': indent xs
indent (x   :xs) = x        : indent xs

-}
