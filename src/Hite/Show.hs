
module Hite.Show() where

import Hite.Type
import List
import General.General
import Data.Predicate


instance Show Hite where
    show (Hite datas funcs) = unlines (map show datas ++ map show funcs)


instance Show Func where
    show (Func name args expr pos) =
        "\n" ++ name ++ concatMap (' ':) args ++
        " @(" ++ pos ++ ")" ++
        " = " ++ show expr

instance Show Expr where
    show x = f False 0 x
        where
            brack True x = "(" ++ x ++ ")"
            brack False x = x
            
            f b i (Var name) = name
            f b i (Unknown) = "Unknown"
            f b i (Bottom) = "_|_"
            f b i (RepeatNow) = "***"
            f b i (Repeat expr alt) = brack b $ f b i expr ++ " | " ++ f b i alt
            f b i (Msg s) = show s
            f b i (Error s) = brack b $ "Error " ++ show s
            
            f b i (MCase opts) = concatMap show opts

            f b i (Sel expr arg) = f True i expr ++ "." ++ arg
            f b i (Make name args) = f b i (Call (CallFunc name) args)
            f b i (Prim name args) = f b i (Call (CallFunc $ "prim!_" ++ name) args)
            f b i (CallFunc name) = name
            f b i (Call (CallFunc name) []) = name
            f b i (Call name args) = brack b $ concat $ intersperse " " $
                                     map (f True i) (name:args)

            f b i (Case cond opts) = "case " ++ show cond ++ " of\n" ++
                                     if null opts then "    {- NO OPTIONS! -}" else
                                     (init $ unlines $ map (g (i+4)) opts)

            g i ("",b) = g i ("<*>",b)
            g i (a,b) = replicate i ' ' ++ a ++ " -> " ++ f False i b
            
            h i alt Nothing = "( *** | " ++ f True i alt ++ ")"
            h i alt (Just x) = f True i x


instance Show MCaseAlt where
    show (MCaseAlt cond expr) = "\n  | " ++ show cond ++ " = " ++ show expr
    
    
instance Show MCaseOpt where
    show (MCaseOpt expr ctor) = show expr ++ "{" ++ ctor ++ "}"
