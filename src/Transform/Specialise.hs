
module Transform.Specialise where

import Transform.Type
import General.General
import Data.List
import Control.Exception


specialise :: IHite -> IHite
specialise ihite = decodeCell $ reach $ useNames $ allocNames $ drive ihite

        
-- fixed point on all operations
-- apart from the cleanup ones
drive :: IHite -> IHite
drive x = f (simpler x)
    where
        f x = if b then f x2 else x2
            where (b,x2) = oneStep (False,x)
    
        oneStep = liftId simpler . liftMay inliner . liftId simpler . liftMay genSpec . liftId useSpec
    
        liftId f (b, x) = (b, f x)
        liftMay f (b, x) = case f x of {Nothing -> (b, x); Just y -> (True, y)}



applyAll :: (IExpr -> IExpr) -> IHite -> IHite
applyAll f (IHite a b) = IHite a [func{funcExpr = mapOver f (funcExpr func)} | func <- b]


getFunc2 :: IHite -> FuncPtr -> IFunc
getFunc2 (IHite _ funcs) (FuncPtr a b) = headNote ("getFunc2: Cant find: " ++ show (FuncPtr a b)) res
    where res = [func | func@(Func _ _ _ [(TweakExpr c,d)]) <- funcs, c == b && d == a]


decodeCell :: IHite -> IHite
decodeCell ihite = applyAll f ihite
    where
        f (Cell (FuncPtr name _) 0 args) = Call name args
        f x = x


reach :: IHite -> IHite
reach ihite@(IHite a b) = IHite a $ filter (\x -> funcName x `elem` keep) b
    where
        keep = fixSet f ["main"]
        
        f x = nub [x | Cell (FuncPtr x _) _ _ <- allOver $ funcExpr $ getFunc ihite x]



allocNames :: IHite -> IHite
allocNames (IHite a b) = IHite a (zipWith f [1..] b)
    where
        f n func | last nam == '?' = func{funcName = init nam ++ "~" ++ show n}
                 | otherwise = func
            where nam = funcName func


useNames :: IHite -> IHite
useNames ihite@(IHite _ funcs) = applyAll f ihite
    where
        f (Cell name n xs) = Cell (FuncPtr (funcName $ getFunc2 ihite name) []) n xs
        f x = x
        


-- find a fixed point always
-- return Nothing if no changes
inliner :: IHite -> Maybe IHite
inliner ihite = if g res == g ihite then Nothing else Just res
    where
        g (IHite a b) = b
    
        res = applyAll f ihite
    
        f (Cell name 0 xs) | canInline func = mapOver f $ mapOver g (funcExpr func)
            where
                func = getFunc2 ihite name 
                g (Var i) = xs !! i
                g x = x
        f x = x

        canInline (Func name _ body [(TweakExpr a,b)]) =
            case body of
                (Cell nam 0 xs) | nam /= name && all isSimple xs -> True
                (Make ('(':_) xs) -> True
                _ -> isSimple body
            where name = FuncPtr b a
        
        isSimple (Var x) = True
        isSimple (Sel x y) = isSimple x
        isSimple x = False
                



-- simple transforms, are each individual
-- do not depend on the syntactic info in the rest of the program!
-- guarantee to find a fixed point in one single iteration
simpler :: IHite -> IHite
simpler ihite = applyAll f ihite
    where
        -- simple syntax
        f (Apply (Cell x n xs) (y:ys)) | n >= 1 = f $ Apply (Cell x (n-1) (xs++[y])) ys
        f (Apply x []) = x
    
        -- more involved
        f (Sel (Make name args) arg) | ctorName x == name = args !! cargPos x
            where x = getCArg ihite arg

        f x = x
        


-- only return Just if you actually did something
-- needs to fixpoint because there may be a specialised instance
-- that does not have any concrete implementation - i.e. was inlined
-- itself!
genSpec :: IHite -> Maybe IHite
genSpec ihite = f (genSpecOne ihite)
    where
        f Nothing = Nothing
        f (Just x) = Just $ g x
        
        g x = case genSpecOne x of
                  Nothing -> x
                  Just y -> g y
        

genSpecOne :: IHite -> Maybe IHite
genSpecOne ihite@(IHite datas funcs) = if null newFuncs then Nothing else Just res
    where
        res = IHite datas (map f newFuncs ++ funcs)
        newFuncs = nub reqFuncs \\ haveFuncs
        haveFuncs = [FuncPtr b a | func <- funcs, let [(TweakExpr a,b)] = funcTweaks func]
        reqFuncs = [x | func <- funcs, Cell x _ _ <- allOver (funcExpr func)]
        
        f (FuncPtr name args) = Func (name ++ "?") [0..argCount-1] body [(TweakExpr args,name)]
            where
                Func _ origArgs origBody _ = getFunc ihite name
                (argCount, args2) = giveNumbers args
                body = Apply (mapOver g origBody) (drop (length origArgs) args2)
                
                g (Var i) = args2 !! i
                g x = x



-- automatically finds a fixed point
useSpec :: IHite -> IHite
useSpec ihite = applyAll f ihite
    where
        f (Apply (Cell x 0 args) (e:xtra)) = if null xtra then res else f $ Apply res xtra
            where res = Cell (addArg x) 0 (args++[e])
        
        f cell@(Cell{}) = specFunc cell
        
        f x = x



addArg :: FuncPtr -> FuncPtr
addArg (FuncPtr name args) = FuncPtr name (args ++ [Var 0])
--    where fresh = maximum $ 0:[a+1 | arg <- args, Var a <- allOver arg]


specFunc (Cell (FuncPtr name specs) n args) = Cell (FuncPtr name (g specs)) n (concat newargs)
    where
        (newspecs, newargs) = unzip $ map f args
    
        -- how would you like to specialise
        f :: IExpr -> (IExpr, [IExpr])
        f (Make n ys) = (Make n (map (const $ Var 0) ys), ys)
        f (Cell x n xs) | n /= 0 = (Cell x n [], xs)
        f x = (Var 0, [x])
    
    
        -- now weave the specialisations back in
        g :: [IExpr] -> [IExpr]
        g x = assertNote (show (x,n2,x2,ys)) (n2 == length ys) $ map (mapOver g2) x2
            where
                ys = newspecs ++ replicate n (Var 0)
                (n2,x2) = giveNumbers x
                
                g2 (Var i) = ys !! i
                g2 x = x



-- replace all Var 0's with Var 0..n
-- return the count (i.e. the next free)
giveNumbers :: [IExpr] -> (Int, [IExpr])
giveNumbers xs = f1 xs 0
    where
        f1 (x:xs) n = (c, b:d)
            where
                (a,b) = f2 x n
                (c,d) = f1 xs a
        f1 [] n = (n, [])
        
        f2 (Var 0) n = (n+1, Var n)
        f2 x n = (a, setChildren x b)
            where (a,b) = f1 (getChildren x) n
