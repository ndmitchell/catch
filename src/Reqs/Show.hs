
module Reqs.Show where


import Reqs.Type
import Reqs.Path
import Pred.Show
import Pred.Type
import General.General

import RegExp.Parse -- why? i don't know why this is needed...

import List
import Maybe
import Char


instance Show Req where
    show r = case r of
            (Req expr regs opts) -> "<" ++ inline (output expr) ++ "," ++ pathPretty regs ++ "," ++ strSet opts ++ ">"
            (ReqAll on within) -> "(\\forall " ++ on ++ ", " ++ show within ++ ")"
        where
            inline x = case lines x of
                            [x] -> x
                            xs -> "{" ++ (concat $ intersperse "; " $ map (dropWhile isSpace) xs) ++ "}"



-- cannot show a Req which is not Nothing
prettyReqs :: Reqs -> String
prettyReqs reqs = unlines (map g reps) ++
                  prettyPredBy id (mapPredLitChange f reqs)
    where
        exprs = [x | Req x _ _ <- allPredLit reqs]
        (keepexpr, repexpr) = partition h (nub exprs)
        vars = map (:[]) (['x'..'z'] ++ ['a'..'v']) ++ map (\x -> 'w' : show x) [2..]
        reps = zip repexpr vars
        rens = [(x, output x) | x <- keepexpr] ++ reps
        
        h x = null (exprs \\ [x])

    
        f :: Req -> String
        f (Req expr regs opts) =
            (fromJust $ lookup expr rens) ++
            (if pathIsLambda regs then "" else "." ++ pathPretty regs) ++
            strSet opts
    
        f x = show x
    
        g (expr, var) = "var " ++ var ++ " := " ++ output expr
