    
module Hite.Consts(consts) where

import Hite.Type
import Hite.Show
import Hite.Normalise

import List
import General.General
import Maybe

import Debug.Trace


{-
    Algorithm:
    Partially evaluate an application if one of the arguments are entirely
    non-dependant on the variables
-}


consts :: Hite -> Hite
consts bad_hite = hite{funcs = mapExpr g allFuncs}
    where
        hite = normalise bad_hite
    
        allFuncs = funcs hite ++ map snd res
        res = f 1 (funcs hite) []
    
        f n [] done = done
        f n todo done = -- trace (show newDone) $
                    f (n+1) (map snd newFuns) (done ++ newFuns)
            where
                newEval = concatMap slices $ nub (canEvaluate hite todo)
                newDone = filter (\x -> not $ any (covered x) (map fst done)) newEval
                newFuns = [(x, genEvaluate hite x name) | (i,x@(n2,_)) <- zip [1..] newDone,
                           name <- [n2 ++ "_SPEC_" ++ show n ++ "_" ++ show i]]

                -- is x covered already by y
                covered :: (FuncName, [(Int, Expr)]) -> (FuncName, [(Int, Expr)]) -> Bool
                covered (a,as) (b,bs) = a == b && length as == length bs && and (zipWith (=<=) as bs)
                    where
                        (a1,a2) =<= (b1,b2) = a1 == b1 && any (b2==) (allExpr a2)
                
                slices :: (FuncName, [(Int, Expr)]) -> [(FuncName, [(Int, Expr)])]
                slices (x, xs) = map ((,) x) (filter (not . null) $ selections xs)
                

        g (Call (CallFunc name) args) | not (null poss) = Call (CallFunc newName) callArgs
            where
                callArgs = [a | (i,a) <- zip [0..] args, not (i `elem` map fst newArgs)]
                ((newName,newArgs):_) = poss
                poss = sortBy cmp [(funcName func, pargs) | ((n2, pargs),func) <- res, n2 == name, all check pargs]
                
                check (pos,val) = length args > pos && (args !! pos) == val
                cmp a b = compare (length (snd b)) (length (snd a)) -- sort in reverse order
                
        
        g x = x


selections (x:xs) = selections xs ++ map (x:) (selections xs)
selections [] = [[]]

{-

applyEvaluate :: Hite -> Hite
applyEvaluate n hite = partialEval $ normalise $ mapExpr f (hite{funcs = funcs hite ++ newFuncs})
    where
        newFuncs = map fst res
        renames = map snd res
    
        res = [ (func, (newName, name, pos, arg)) |
                    (ind, (name, pos, arg)) <- zip [1..] (canEvaluate hite),
                    newName <- [name ++ "_EVAL_" ++ show n ++ "_" ++ show ind],
                    Just func <- [genEvaluate hite newName (name, pos, arg)]]

        
        f (Call (CallFunc name) args) | not (null poss) = Call (CallFunc newName) (remove pos args)
            where
                ((pos,newName):_) = poss
                poss = [(pos,newName) | (newName, name2, pos, arg) <- renames,
                        name == name2, length args > pos, args !! pos == arg]

        f x = x    
-}


canEvaluate :: Hite -> [Func] -> [(FuncName, [(Int, Expr)])]
canEvaluate hite funcs = concatMap f (allExpr funcs)
    where
        f (Call (CallFunc name) args) = if null poss then [] else [(name, poss)]
            where
                poss = [(pos,arg) | (pos,arg) <- zip [0..max_arg-1] args, varFree arg]
                max_arg = length $ funcArgs $ getFunc hite name
        f _ = []


genEvaluate :: Hite -> (FuncName, [(Int, Expr)]) -> String -> Func
genEvaluate hite (name, pargs) newName = 
        Func newName newArgs newBody Star
    where
        Func _ args body _ = getFunc hite name
        
        poss = map fst pargs
        newArgs = [x | (n,x) <- zip [0..] args, not (n `elem` poss)]
        rename = [(a,x) | (i,a) <- zip [0..] args, Just x <- [lookup i pargs]]
        
        newBody = mapExpr f body
        
        f (Var x _) | isJust res = fromJustNote "genEvaluate" res
            where res = lookup x rename
        f x = x




{-
partialEval :: Hite -> Expr -> Expr
partialEval hite expr = normalise $ mapExpr f $ mapExpr f $ mapExpr f $ normalise expr
    where
        f (Sel (Call (CallFunc name) args) path) | all varFree args =
                if len /= length args then
                    error $ show ("partialEval", expr, (Sel (Call (CallFunc name) args) path), len, getFunc hite name)
                else
                    Sel (expand hite name args) path
            where len = length $ funcArgs $ getFunc hite name
        
        f (Sel (Make name args) path) | name == name2 = args !! pos
            where
                Ctor name2 args2 = getCtorFromArg hite path
                pos = head [i | (i,a) <- zip [0..] args2, a == path]
                
        f (Case (Make name _) opts) = if null o then Call (CallFunc "catch_bot") [] else snd (head o)
            where o = filter ((==) name . fst) opts
                
        f x = x
        
        
expand :: Hite -> FuncName -> [Expr] -> Expr
expand hite name params = mapExpr f body
    where
        (Func _ args body _) = getFunc hite name
        
        rename = zip args params

        f (Var a _) = fromJustNote (show ("expand",name,params)) $ lookup a rename
        f x = x
-}

varFree x = null [() | Var _ _ <- allExpr x] && length (allExpr x) < 15


