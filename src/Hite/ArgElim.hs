
-- Eliminate useless arguments

module Hite.ArgElim(cmd) where

import Hite.Type
import Data.List
import Data.Maybe


cmd = cmdHitePure (const argElim) "arg-elim"
            "Eliminate useless arguments"


argElim :: Hite -> Hite
argElim hite@(Hite datas funcs) = Hite datas newfuncs
    where
        newfuncs = map (newFunc hite) res ++ useFuncs res funcs
        res = concatMap deadArgs funcs


useFuncs :: [(FuncName, [Int])] -> [Func] -> [Func]
useFuncs res hite = mapExpr f hite
    where
        f x@(Call (CallFunc n) xs) =
            case lookup n res of
                Just dead | last dead < length xs -> Call (CallFunc (n ++ "_ARG")) (dropDead dead xs)
                _ -> x
        f x = x
            


deadArgs :: Func -> [(FuncName, [Int])]
deadArgs (Func name args body _) = [(name, unusedPos) | not $ null unused]
    where
        used = nub [x | Var x <- allExpr body]
        unused = args \\ used
        unusedPos = map (\x -> fromJust $ elemIndex x args) unused


newFunc :: Hite -> (FuncName, [Int]) -> Func
newFunc hite (name, dead) = Func (name ++ "_ARG") (dropDead dead args) body ex
    where (Func _ args body ex) = getFunc hite name


dropDead :: [Int] -> [a] -> [a]
dropDead del xs = [x | (n,x) <- zip [0..] xs, not $ n `elem` del]

