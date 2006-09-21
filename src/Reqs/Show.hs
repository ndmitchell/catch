
module Reqs.Show where


import Reqs.Type
import Reqs.Path
import General.General

import RegExp.Parse -- why? i don't know why this is needed...

import List
import Maybe
import Char

import Data.Predicate


instance Show Req where
    show (Req expr regs opts _) = "<" ++ inline (show expr) ++ "," ++ pathPretty regs ++ "," ++ strSet opts ++ ">"
        where
            inline x = case lines x of
                [x] -> x
                xs -> "{" ++ (concat $ intersperse "; " $ map (dropWhile isSpace) xs) ++ "}"


instance Show ReqAll where
    show (ReqAll on within) = "(\\forall " ++ on ++ ", " ++ show within ++ ")"



-- cannot show a Req which is not Nothing
prettyReqs :: Reqs -> String
prettyReqs reqs = "prettyReq's no longer works" {- unlines (map g reps) ++
                  prettyPredBy id (mapPredLitChange f reqs)
    where
        exprs = [x | Req x _ _ _ <- allPredLit reqs]
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
-}
