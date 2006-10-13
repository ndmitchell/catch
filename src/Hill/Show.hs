
module Hill.Show where

import Hill.Type
import List
import General.General


instance Show ValueHill where
    show (ValueHill x) = show x
    show ValueNone = ""


instance Show Hill where
    show (Hill datas funcs) = unlines (map show datas ++ map show funcs)
    

instance Show Func where
    show (Func name args expr) =
        "\n" ++ name ++ concatMap ((' ':) . show) args ++
        " = " ++ show expr

instance Show Expr where
    show x = f False 0 x
        where
            brack True x = "(" ++ x ++ ")"
            brack False x = x
            
            f b i x = case x of
                Var name -> '@' : show name
                Fun x -> x
                Const x -> show x

                Call x [] -> x
                Call x xs -> brack b $ x ++ concatMap ((' ':) . f True i) xs
                Make x xs -> f b i (Call x xs)
                Prim x xs -> f b i (Call x xs)

                Sel x xs -> f True i x ++ "." ++ xs
                
                Case cond opts -> "case " ++ show cond ++ " of\n" ++
                                     if null opts then "    {- NO OPTIONS! -}" else
                                     (init $ unlines $ map (g (i+4)) opts)
                    where
                        g i (Default b) = g i (Alt (ACtor "_") b)
                        g i (Alt a b) = replicate i ' ' ++ show a ++ " -> " ++ f False i b

                Let bind x -> brack b $ "let\n" ++ (unlines $ map (g (i+4)) bind) ++
                                        replicate i ' ' ++ "in\n" ++ replicate (i+4) ' ' ++ f False (i+4) x
                    where
                        g i (lhs, rhs) = replicate i ' ' ++ "@" ++ show lhs ++ " = " ++ f False (i+4) rhs
                
                Lambda n x -> brack b $ "\\" ++ show n ++ " -> " ++ show x
                Apply x [] -> f b i x
                Apply x xs -> "{" ++ (concat $ intersperse " " $ map (f True i) (x:xs)) ++ "}"
          
                Error x -> f b i $ Call "@error" [x]


instance Show Const where
    show x = case x of
        AInt y -> show y
        AInteger y -> show y
        AFloat y -> show y
        ADouble y -> show y
        AChar y -> show y
        AString y -> show y
        ACtor y -> y
