{-|
    HACKS:
    freeze only pays attention to the first argument
    reverse is included in specialise, really it should be made
        to reach a fixed point
-}
    
module Hite.Specialise(specialise) where

import Hite.Type
import Hite.Kind
import Hite.Normalise
import List
import General
import Maybe


type Template = (FuncName, [Int])
type Instance = (Template, [FuncName])


specialise :: Hite -> Hite
specialise bad_hite = f [] hite
        -- (instances able hite) [] nhite{funcs = (funcs nhite) ++ fullGenerate inst []}
    where
        hite = normalise bad_hite
        able = specialiseable hite
        
        
        f :: [Instance] -> Hite -> Hite
        f done hite = if null todo then hite else f (todo ++ done) res2
            where
                todo = instances hite able \\ done
                res = hite{funcs = (funcs hite) ++ map (generate hite) todo}
                res2 = repoint res todo

{-

        nhite = normalise hite
        inst = nub $ concatMap (getInst (funcs nhite)) able
        
        
        getInst funcs (func,pos) = [(func,zip pos x) | x <- instances funcs (func, pos)]
        
        
        fullGenerate [] done = []
        fullGenerate todo done = newfuncs ++ fullGenerate newtodos newdone
            where
                newdone = todo ++ done
                newfuncs = map (generate nhite) todo
                newtodos = (nub $ concatMap (getInst newfuncs) able) \\ newdone
     -}   
        
        
    {-
    f $ f $ f (Hite ds (reverse fs))
    where
        Hite ds fs = f hite
    
        f x = foldl freeze x (specialiseable x)
        -}
{-


freeze :: Hite -> (FuncName, [FuncArg]) -> Hite
freeze hite (name, arg:_) = hite{funcs = oldfuncs ++ newfuncs}
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
        genName x = name ++ "_SPEC_" ++ x
-}


newName :: Instance -> String
newName ((func, _), args) = func ++ "_SPEC" ++ concatMap ('_':) args


repoint :: Hite -> [Instance] -> Hite
repoint hite is = mapExpr f hite
    where
        f (Call (CallFunc a) bs) | isJust mInst &&
                                   matchInst inst bs
                = Call (CallFunc (newName inst)) [x | (n,x) <- zip [0..] bs, not (n `elem` pos)]
            where
                Just inst@((_, pos),_) = mInst
                mInst = getInstance a
        f x = x
        
        getInstance x = case [i | i@((name,_),_) <- is, x == name] of
                            [] -> Nothing
                            [x] -> Just x
        
        matchInst ((_, pos), val) items = and $ zipWith g pos val
            where g p v = (items !! p) == CallFunc v


-- generate a new function to satisfy the template
generate :: Hite -> Instance -> Func
generate hite i@((func, pos), rep) = Func newname newargs newbody Star
    where
        newname = newName i
        newargs = [x | (n, x) <- zip [0..] args, not (n `elem` pos)]
        newbody = mapExpr f body
    
        rename = [(args !! n, name) | (n,name) <- zip pos rep]
        Func _ args body k = getFunc func hite
        
        f (Var x y) = case lookup x rename of
                           Just z -> CallFunc z
                           Nothing -> Var x y
        f x = x



-- find all the places you could use all the templates function
instances :: Hite -> [Template] -> [Instance]
instances hite ts = nub $ concatMap g ts
    where
        g t = concatMap (f t) (funcs hite)

        f t@(func, pos) (Func name args body _)
            | name == func = []
            | otherwise    =
             [(t, map callName ars) | Call (CallFunc n) as <- allExpr body, n == func,
                    ars <- [map (as !!) pos], all isCallFunc ars]
        


-- | Which functions can be specialised, and in which arguments.
--   Arguments are 0 indexed, all have at least one specialiseable argument
--   No function appears more than once
specialiseable :: Hite -> [Template]
specialiseable hite = concatMap f (funcs $ kind hite)
    where
        f (Func name []   body _         ) = []
        f (Func name args body (Arrow ks)) = 
                if null sargs then [] else [(name, sargs)]
            where
                sargs = map (\(a,b,c) -> a) $ filter g (zip3 [0..] args ks)
                
                g (n, arg, Star) = False
                g (n, arg, _) = and items
                    where
                        items = [varArg (args !! n) == arg |
                                      Call (CallFunc me) args <- allExpr body,
                                      me == name]

{-

foldl specialise hite $ filter (canSpecialise hite) $ getHigherOrder hite


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
        newfunc replace = Func newName (remArg funcArgs) (mapExpr f funcBody)
            where
                newName = genName replace
            
                f c@(Call _ args) | getArgVar c == [replace] =
                    Call (CallFunc newName) (remArg args)
                f (Var x []) | x == arg = CallFunc replace
                f x = x
                                                    
        
        useful = nub [argVal | Func func _ expr <- funcs hite,
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
-}

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
