
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
    show _ = "todo"

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
