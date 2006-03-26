    
module Hite.MakeMove(cmd, makeMove) where

-- Move make's around a bit
-- case Make of can be taken out (provided it won't error)
-- Make.sel can be reduced

import Hite.Type
import Hite.Show
import Maybe
import General.General

cmd = cmdHitePure (const makeMove) "make-move"
            "Move constructors to the best place"



{-
    Algorithm:
    Partially evaluate an application if one of the arguments are entirely
    non-dependant on the variables
-}


makeMove :: Hite -> Hite
makeMove hite = mapExpr f hite
    where
        f (Case on opts) | isJust ctor && isJust opt = fromJust opt
            where
                ctor = getMake hite on
                opt = lookup (fromJust ctor) opts
        
        f (Sel x path) | ctor == Just want = pickSel hite x path
            where
                ctor = getMake hite x
                want = ctorName $ getCtorFromArg hite path
        
        f x = x


-- is the expression a direct Make
getMake :: Hite -> Expr -> Maybe CtorName
getMake hite x = case getValue hite x of
                    Make name _ -> Just name
                    _ -> Nothing

getValue :: Hite -> Expr -> Expr
getValue hite (Call (CallFunc name) params)
        | length params == length args
        = expand hite name params
    where
        Func _ args body = getFunc hite name
getValue hite x = x


-- pick up a given selector
pickSel :: Hite -> Expr -> CtorArg -> Expr
pickSel hite x path = params !! pos
    where
        Make n params = getValue hite x
        Ctor _ args = getCtor hite n
        pos = head [i | (i,a) <- zip [0..] args, a == path]


expand :: Hite -> FuncName -> [Expr] -> Expr
expand hite name params = mapExpr f body
    where
        (Func _ args body) = getFunc hite name
        
        rename = zip args params

        f (Var a _) = fromJustNote (show ("expand",name,params)) $ lookup a rename
        f x = x


{-





evaluate bad_hite = hite{funcs = mapExpr g allFuncs}
    where
        hite = normalise bad_hite
    
        allFuncs = funcs hite ++ map snd res
        res = f 1 (funcs hite) []
    
        f 9 todo done = done
        f n todo done = f (n+1) (map snd newFuns) (done ++ newFuns)
            where
                newDone = nub (canEvaluate hite todo) \\ map fst done
                newFuns = [(x, genEvaluate hite x name) | (i,x@(n2,_)) <- zip [1..] newDone,
                           name <- [n2 ++ "_SPEC_" ++ show n ++ "_" ++ show i]]

        g (Call (CallFunc name) args) | not (null poss) = Call (CallFunc newName) callArgs
            where
                callArgs = [a | (i,a) <- zip [0..] args, not (i `elem` map fst newArgs)]
                ((newName,newArgs):_) = poss
                poss = sortBy cmp [(funcName func, pargs) | ((n2, pargs),func) <- res, n2 == name, all check pargs]
                
                check (pos,val) = length args > pos && (args !! pos) == val
                cmp a b = compare (length (snd b)) (length (snd a)) -- sort in reverse order
                
        
        g x = x

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
                poss = [(pos,arg) | (pos,arg) <- zip [0..max_arg] args, varFree arg]
                max_arg = length $ funcArgs $ getFunc hite name
        f _ = []


genEvaluate :: Hite -> (FuncName, [(Int, Expr)]) -> String -> Func
genEvaluate hite (name, pargs) newName = 
        (if newName == "  mapM_SPEC_1_15" then error . show else id) $
        Func newName newArgs newBody Star
    where
        Func _ args body _ = getFunc hite name
        
        poss = map fst pargs
        newArgs = [x | (n,x) <- zip [0..] args, not (n `elem` poss)]
        rename = [(a,x) | (i,a) <- zip [0..] args, Just x <- [lookup i pargs]]
        
        newBody = partialEval hite $ mapExpr f body
        
        f (Var x _) | isJust res = fromJustNote "genEvaluate" res
            where res = lookup x rename
        f x = x




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
        
        
varFree x = null [() | Var _ _ <- allExpr x] && length (allExpr x) < 15


expand :: Hite -> FuncName -> [Expr] -> Expr
expand hite name params = mapExpr f body
    where
        (Func _ args body _) = getFunc hite name
        
        rename = zip args params

        f (Var a _) = fromJustNote (show ("expand",name,params)) $ lookup a rename
        f x = x
-}
