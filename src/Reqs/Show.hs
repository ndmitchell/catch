
module Reqs.Show where


import Reqs.Type
import Reqs.Path
import Pred.Show
import Pred.Type
import General.General

import List
import Maybe
import Char


instance Show Req where
    show (Req expr regs opts) =
        "<" ++ inline (show expr) ++ "," ++ show regs ++ "," ++ strSet opts ++ ">"
        where
            inline x = case lines x of
                            [x] -> x
                            xs -> "{" ++ (concat $ intersperse "; " $ map (dropWhile isSpace) xs) ++ "}"



prettyReqs :: Reqs -> String
prettyReqs reqs = unlines (map g rens) ++
                  prettyPredBy id (mapPredLitChange f reqs)
    where
        exprs = nub $ map reqExpr $ allPredLit reqs
        vars = map (:[]) (['x'..'z'] ++ ['a'..'v']) ++ map (\x -> 'w' : show x) [2..]
        rens = zip exprs vars
    
        f :: Req -> String
        f (Req expr regs opts) =
            (fromJust $ lookup expr rens) ++
            (if pathIsLambda regs then "" else "." ++ pathPretty regs) ++
            strSet opts
    
    
        g (expr, var) = "var " ++ var ++ " := " ++ show expr
