
module Hite.Firstify(firstify) where

import Hite.Type
import List
import General


firstify :: Hite -> Hite
firstify hite = foldl specialise hite $ filter (canSpecialise hite) $ getHigherOrder hite


specialise :: Hite -> (FuncName, FuncArg) -> Hite
specialise hite (name, arg) = hite{funcs = oldfuncs ++ newfuncs}
    where
        pos = getArgPos name arg hite
        (Func _ funcArgs funcBody _) = getFunc name hite
        
        
        oldfuncs = map (\x -> x{body = mapExpr bind (body x)}) (funcs hite)
        bind c@(Call _ args) | ar /= [] && (arr `elem` useful) =
            Call (CallFunc $ genName arr) (remArg args)
            where
                ar = getArgVar c
                arr = head ar
        bind x = x
        
        
        newfuncs = map newfunc useful
        newfunc replace = Func newName (remArg funcArgs) (mapExpr f funcBody) Star
            where
                newName = genName replace
            
                f c@(Call _ args) | getArgVar c == [replace] =
                    Call (CallFunc newName) (remArg args)
                f (Var x []) | x == arg = CallFunc replace
                f x = x
                                                    
        
        useful = nub [argVal | Func func _ expr _ <- funcs hite,
                               func /= name,
                               call <- allExpr expr,
                               argVal <- getArgVar call]

        getArgVar :: Expr -> [String]
        getArgVar c@(Call (CallFunc nam) args) |
            nam == name = 
            case callArg c pos of
                 Just (CallFunc n) -> [n]
                 _ -> []
        getArgVar _ = []
        
        remArg args = take (pos-1) args ++ drop pos args
        genName x = name ++ "_FIRST_" ++ x
                                                      


-- get a list of all higher order functions
getHigherOrder :: Hite -> [(FuncName, FuncArg)]
getHigherOrder hite = nub [(funcName f, arg) | f <- funcs hite, Call (Var arg []) _ <- allExpr $ body f]


canSpecialise :: Hite -> (FuncName, FuncArg) -> Bool
canSpecialise hite (name, arg) = sum (map f $ allExpr $ body $ getFunc name hite) == 0
    where
        pos = getArgPos name arg hite
        
        f (Var n path) | n == arg = 0+1
        f x@(Call (CallFunc func) args) | name == func &&
                                        callArg x pos == Just (Var arg []) = 0-1
        f (Call (Var n []) _) | n == arg = 0-1
        f _ = 0
        

recursiveCalls :: Hite -> FuncName -> [Expr]
recursiveCalls hite func = [x |
    x@(Call (CallFunc name) args) <- allExpr $ body $ getFunc func hite,
    name == func]


{-




import Hite
import List
import Maybe


paramable :: Hite -> Hite
paramable hite = bind hite (getParams hite)
    where
        bind h [] = h
        bind h (x:xs) = bind (applyParam h x) xs





applyParam :: Hite -> (FuncName, Int) -> Hite
-- find all calls (not recursive)
-- expand out
applyParam hite (name, arg) = hite{funcs = concatMap f (funcs hite)}
    where
        body = getFunc hite name
    
        f (nam, expr) | nam == name = [(nam, expr)]
                      | otherwise = (nam, newexpr) : map snd newfuncs
            where
                newfuncs = zipWith (genFunc nam) (filter canParam (allExps expr)) [1..]
                newexpr = if null newfuncs then expr else mapExp rebind expr
                
                (free, (newname, _)) = head newfuncs
                rebind x | canParam x = Call (CallFunc newname) (free ++ dropArg (callArgs x))
                rebind x = x


        dropArg xs = take (arg-2) xs ++ drop arg xs
                
        
        canParam (Call (CallFunc a) args) = a == name && length args >= arg
        canParam _ = False
        
        
        genFunc nam (Call _ args) n = (freeVars, (name2, expr))
            where
                name2 = name ++ "_" ++ nam ++ "_" ++ show n
                expr = mapExp renBody body
                freeVars = nub $ filter isVar $ allExps argBody
                argBody = args !! (arg-1)
                argBody2 = mapExp renArg argBody
                
                renBody (Var n m) | n == arg = argBody2
                    | otherwise = Var (n + (if n > arg then -1 else 0) + length freeVars) m
                renBody (Call (CallFunc a) args) | a == name = Call (CallFunc name2)
                    (map asVar [1..length freeVars] ++ dropArg args)
                renBody x = x
                
                freeVarsInd = zip freeVars (map asVar [1..])
                renArg x@(Var _ _) = fromJust $ lookup x freeVarsInd
                renArg x = x
        
        
asVar n = Var n []        
        
        
        


getParams :: Hite -> [(FuncName, Int)]
getParams hite = concatMap f (funcs hite)
    where
        f (name, expr) = map ((,) name) $
                if null res || last name == '_' then []
                -- concatMap allVar (allExps expr)
                else foldr1 intersect (map g res)
            where res = recs (name, expr)
        
        g (Call _ args) = catMaybes $ zipWith h args [1..]
        h (Var n []) m | n == m = Just n
        h _ _ = Nothing
        
        
        allVar (Var n _) = [n]
        allVar _ = []
        
        
        
        recs (name, expr) = filter f (allExps expr)
            where
                f x@(Call (CallFunc a) _) = a == name
                f _ = False


-}
