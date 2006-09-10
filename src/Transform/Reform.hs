
module Transform.Reform(reform) where

import Transform.Driver
import Transform.Type
import General.General
import Data.List
import Data.Maybe
import Debug.Trace



reform :: IHite -> IHite
reform ihite = manyPass $ applyTweak ihite


applyTweak x = case applyExprTweak x of
                    Nothing -> reachHite x
                    Just x -> applyTweak x



type Desc = (FuncName, [Template])

data Template = TemplateCtor CtorName Int -- number of free
              | TemplateFunc FuncName [Bool] -- true is a free argument, false is a given
              | TemplateId
              deriving (Eq,Show)


halfPass x = snd $ onePass x

onePass :: IHite -> (Bool,IHite)
onePass x = (changed, reachHite $ applyTweak $ lambdaLift $ applyTweak x2)
    where
        (x1@(IHite _ f1),arg) = eliminateWeak x $ calcArgs x
        x2@(IHite _ f2) = specialiseStrong x1 arg
        changed = length f1 /= length f2


manyPass x = if a then manyPass b else b
    where (a,b) = onePass x



lambdaLift :: IHite -> IHite
lambdaLift ihite@(IHite datas funcs) = IHite datas (map f funcs)
    where
        f (Func name args (Lambda xs body) rest) = f (Func name (args++xs) body rest)
        f (Func name args body rest) = Func name args (mapOver g body) rest
        
        getArity :: FuncName -> Int
        getArity name = h $ funcExpr $ getFunc ihite name
            where
                h (Lambda xs y) = length xs + h y
                h x = 0
        
        g (Call x xs) | arity /= 0 = Lambda fresh (Call x (xs ++ map Var fresh))
            where
                arity = getArity x
                fresh = take arity $ freshFree (Call x xs)
        g x = x



specialiseStrong :: IHite -> ArgWeights -> IHite
specialiseStrong ihite@(IHite datas funcs) args = IHite datas $ f funcs []
    where
        f [] done = []
        f (f1:uncs) done = useTemplate f1 newdone : f (uncs ++ map genTemplate newNam) newdone
            where
                used = map snd done ++ map funcName funcs
                new = collectTemplate f1 \\ map fst done
                newNam = zip new (newNames used $ map fst new)
                newdone = done ++ newNam

    
        newNames :: [FuncName] -> [FuncName] -> [FuncName]
        newNames used [] = []
        newNames used (x:xs) = next : newNames (next:used) xs
            where next = head $ [takeWhile (/='~') x ++ "~" ++ show i | i <- [1..]] \\ used
    

        genTemplate :: (Desc,FuncName) -> IFunc
        genTemplate ((func,spec),newname) = Func newname (concat newargs) newbody []
            where
                newbody = replaceFree (zip args (zipWith makeArg newargs spec)) body
            
                newargs = f fresh spec
                fresh = freshFree body \\ args
            
                Func _ args body _ = getFunc ihite func
                
                f free [] = []
                f free (x:xs) = a : f b xs
                    where (a,b) = splitAt (countFree x) free
                
                
                makeArg [n] (TemplateId) = Var n
                makeArg ns  (TemplateCtor nam _) = Make nam (map Var ns)
                makeArg ns  (TemplateFunc nam x) = Lambda newfresh (Call nam $ map Var args)
                    where
                        args = f x ns newfresh
                        
                        f (False:bs) (x:xs) ys = x : f bs xs ys
                        f (True :bs) xs (y:ys) = y : f bs xs ys
                        f [] [] [] = []
                        
                        newfresh = take xt $ fresh \\ concat newargs
                        xt = length $ filter (==True) x
                        xf = length x - xt
                
                countFree (TemplateId) = 1
                countFree (TemplateCtor nam n) = n
                countFree (TemplateFunc nam n) = length $ filter (==False) n
                
            
        useTemplate :: IFunc -> [(Desc,FuncName)] -> IFunc
        useTemplate x desc = x{funcExpr = mapOver f (funcExpr x)}
            where
                f (Call x xs) | isJust res && isJust name = Call (fromJust name) (concat $ zipWith g xs (snd jres))
                    where
                        name = lookup jres desc
                        jres = fromJust res
                        res = collectCall x xs
                f x = x
                
                g arg TemplateId = [arg]
                g (Lambda free (Call _ args)) (TemplateFunc name opts) = [a | (a,False) <- zip args opts]
                g (Make _ xs) (TemplateCtor x _) = xs
                

    
        -- figure out what templates can be used
        collectTemplate :: IFunc -> [Desc]
        collectTemplate func = concatMap f $ allOver $ funcExpr func
            where
                f (Call x xs) = maybeToList $ collectCall x xs
                f x = []

        collectCall :: FuncName -> [IExpr] -> Maybe Desc
        collectCall x xs = if all (== TemplateId) res then Nothing else Just (x,res)
            where res = [if a == Strong then b else TemplateId
                        | (a,b) <- zip (lookupNote "collectCall" x args) (map makeTemplate xs)]


        makeTemplate :: IExpr -> Template
        makeTemplate (Make x xs) = TemplateCtor x (length xs)
        
        makeTemplate (Lambda free (Call name args))
                | filter hasLocal args == map Var free
                = TemplateFunc name (map hasLocal args)
            where hasLocal x = [v | Var v <- allOver x] `overlap` free

        makeTemplate _ = TemplateId



eliminateWeak :: IHite -> ArgWeights -> (IHite, ArgWeights)
eliminateWeak (IHite datas funcs) weights = (IHite datas $ map f funcs, map h weights)
    where
        keep name lst = [a | (a,b) <- zip lst (lookupNote "eliminateWeak" name weights), b /= Weak]
    
        f (Func name args body _) = Func name (keep name args) (mapOver g body) []
        
        g (Call x xs) = Call x (keep x xs)
        g x = x
        
        h (x,y) = (x, filter (/= Weak) y)




data Weight = Weak | Normal | Strong
             deriving (Ord,Eq,Show)

type ArgWeights = [(FuncName,[Weight])]


calcArgs :: IHite -> ArgWeights
calcArgs ihite@(IHite datas funcs) = fixp next base
    where
        base = [(funcName func, replicate (length $ funcArgs func) Weak) | func <- funcs]

        next :: [(FuncName,[Weight])] -> [(FuncName,[Weight])]
        next know = map (f know) know

        f :: [(FuncName,[Weight])] -> (FuncName,[Weight]) -> (FuncName,[Weight])
        f know (funcname,_) = (funcname, map h (funcArgs func))
            where
                func = getFunc ihite funcname
                body = funcExpr func
                res = nub $ concatMap tweak $ (Normal,body) : concatMap g (allOver body)
                h i = maximum $ Weak : [a | (a,b) <- res, b == i]
            
                g (Case on xs) = [(Strong, on)]
                g (Call name xs) = zip (lookupNote "calcArgs" name know) xs
                g (Apply x args) = (Strong,x) : zip (repeat Normal) args
                g x = []

        tweak :: (Weight,IExpr) -> [(Weight,Int)]
        tweak (w,Var x) = [(w,x)]
        tweak (w,Call _ x) = [] -- already should have been done
        tweak (Normal,x) = concatMap (\x -> tweak (Normal,x)) (getChildren x)
        tweak (w,x) = []
        


fixp f x = if x == x2 then x else fixp f x2
    where x2 = f x
