
module Hite.Firstify(firstify) where


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

