
module Transform.Specialise where

import Transform.Type
import General.General
import Data.List
import Control.Exception


specialise :: IHite -> IHite
specialise ihite = error $ show $ genSpec $ useSpec $ simpler ihite


applyAll :: (IExpr -> IExpr) -> IHite -> IHite
applyAll f (IHite a b) = IHite a [func{funcExpr = mapOver f (funcExpr func)} | func <- b]



-- simple transforms, are each individual
-- do not depend on the syntactic info in the rest of the program!
simpler :: IHite -> IHite
simpler ihite = applyAll f ihite
    where
        f (Apply (Cell x n xs) (y:ys)) | n >= 1 = f $ Apply (Cell x (n-1) (xs++[y])) ys
        f (Apply x []) = x
        f x = x
        


genSpec :: IHite -> IHite
genSpec ihite@(IHite datas funcs) = IHite datas (map f newFuncs ++ funcs)
    where
        newFuncs = reqFuncs \\ haveFuncs
        haveFuncs = [FuncPtr b a | func <- funcs, let [(TweakExpr a,b)] = funcTweaks func]
        reqFuncs = [x | func <- funcs, Cell x _ _ <- allOver (funcExpr func)]
        
        f (FuncPtr name args) = Func (name ++ "~?") [0..argCount-1] body [(TweakExpr args,name)]
            where
                orig = funcExpr $ getFunc ihite name
                (argCount, args2) = giveNumbers args
                body = mapOver g orig
                
                g (Var i) = args2 !! i
                g x = x




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
