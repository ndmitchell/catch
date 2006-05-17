
module Hite.Show() where

import Hite.Type
import List
import General.General
import Data.Predicate


instance Output Hite where
    output (Hite datas funcs) = unlines (map output datas ++ map output funcs)
    
instance Output Data where
    output (Data name ctors) = "data " ++ name ++ " = " ++ concat (intersperse " | " $ map output ctors)
    
instance Output Ctor where
    output (Ctor name args) = name ++ concatMap (' ':) args
    
instance Output Func where
    output (Func name args expr pos) =
        "\n" ++ name ++ concatMap (' ':) args ++
        " @(" ++ pos ++ ")" ++
        " = " ++ output expr

instance Output Expr where
    output x = f False 0 x
        where
            brack True x = "(" ++ x ++ ")"
            brack False x = x
            
            f b i (Var name) = name
            f b i (Bottom) = "_|_"
            f b i (RepeatNow) = "***"
            f b i (Repeat expr alt) = brack b $ f b i expr ++ " | " ++ f b i alt
            f b i (Msg s) = show s
            
            f b i (MCase opts) = concatMap output opts

            f b i (Sel expr arg) = f True i expr ++ "." ++ arg
            f b i (Make name args) = f b i (Call (CallFunc name) args)
            f b i (CallFunc name) = name
            f b i (Call (CallFunc name) []) = name
            f b i (Call name args) = brack b $ concat $ intersperse " " $
                                     map (f True i) (name:args)

            f b i (Case cond opts) = "case " ++ output cond ++ " of\n" ++
                                     if null opts then "    {- NO OPTIONS! -}" else
                                     (init $ unlines $ map (g (i+4)) opts)

            g i (a,b) = replicate i ' ' ++ a ++ " -> " ++ f False i b
            
            h i alt Nothing = "( *** | " ++ f True i alt ++ ")"
            h i alt (Just x) = f True i x


instance Output MCaseAlt where
    output (MCaseAlt cond expr) = "\n  | " ++ output cond ++ " = " ++ output expr
    
    
instance Output (Pred MCaseOpt) where
    output x = showPredBy f x
        where
            f (expr,ctor) = output expr ++ "{" ++ ctor ++ "}"
