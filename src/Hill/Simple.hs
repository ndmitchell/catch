
module Hill.Simple(cmdsSimple, simplify, simplifyEx, SimplifyOpt(..), normalise, applyFuns, useVectorMake) where

import Hill.Type
import Hill.PrimOp
import Data.List
import Data.Maybe
import General.General


cmdsSimple =
    [hillCmdPure "simplify" (\name hill -> simplify hill hill)
    ,hillCmdPure "normalise" (const normalise)
    ,hillCmdPure "apply-funs" (const applyFuns)
    ,hillCmdPure "vector" (const useVector)
    ,hillCmdPure "novector" (const noVector)
    ,hillCmdPure "int" (const useInt)
    ,hillCmdPure "var-rejoin" (const varRejoin)
    ]


---------------------------------------------------------------------

data SimplifyOpt = NoLet deriving Eq

-- basic simplifications
simplify :: ManipulateHill hill => Hill -> hill -> hill
simplify = simplifyEx []

simplifyEx :: ManipulateHill hill => [SimplifyOpt] -> Hill -> hill -> hill
simplifyEx opts hill x = mapOverHill f x
    where
        yesLet = not $ NoLet `elem` opts
    
        -- use error if you can
        f (Apply (Fun "error") [x]) = Error x
        f (Call "error" [x]) = Error x
        
        -- inline simple lets, @1 = @2
        f (Let binds x) | yesLet && not (null simp) = f $ mkLet complex $ mapOverHill g x
            where
                g (Var x) = case lookup x simp of
                                Nothing -> Var x
                                Just y -> y
                g x = x
            
                (simp, complex) = partition (isVar . snd) binds
        
        -- move applications inwards
        f (Apply (Case on alts) xs) = Case on [alt{altExpr = f (mkApply (altExpr alt) xs)} | alt <- alts]
        f (Apply (Let binds x) xs) = Let binds (f $ Apply x xs)
        -- move selectors inwards
        f (Sel (Let binds x) path) = Let binds (f $ Sel x path)
        f (Sel (Case on alts) path) = Case on [alt{altExpr = f $ Sel (altExpr alt) path} | alt <- alts]
        
        -- discard unused lets
        f (Let binds x) | yesLet && not (null unused) = f $ mkLet used x
            where
                required = requiredFree x
                (used, unused) = partition ((`elem` required) . fst) binds
        
        -- float chains of lets into one (if possible)
        f (Let binds1 (Let binds2 x)) | yesLet && not (null float) = Let (binds1++float) (mkLet nofloat x)
            where
                binded = map fst binds1
                (float, nofloat) = partition (disjoint binded . usedFree . snd) binds2
        
        -- let x = blah in x
        f (Let binds (Var x)) | yesLet && isJust y = fromJust y
            where y = lookup x binds
        
        -- case Ctor of Ctor -> x ==> x
        f (Case (Const x) alts) | not $ null match = head match
                                | not $ null def   = head def
            where
                match = [b | AltConst a b <- alts, a == x]
                def = [x | Default x <- alts]
        
        f (Case on alts) | isJust onCtor && not (null match) = head match
            where
                onCtor = getCtor on
                match = [y | AltCtr x y <- alts, x == fromJust onCtor]
            
                getCtor (Make x xs) = Just x
                getCtor (Ctr x) = Just x
                getCtor (Apply x _) = getCtor x
                getCtor _ = Nothing
        
        -- case x of Alt x -> x.path ==> x.path (where only one ctor)
        f (Case (Var x) [alt]) | f (altExpr alt) = altExpr alt
            where
                f (Sel y _) = f y
                f (Var y) = x == y
                f _ = False
        
        -- (Ctor x y).sel1 ==> x
        f (Sel on path) | isJust res = fromJust res
            where
                carg = getCArg hill path
                (ctor,pos) = (ctorName carg, cargPos carg)
                
                res = g on
                g (Apply (Ctr name) args) = g (Make name args)
                g (Make name args) | name == ctor && length args > pos = Just (args !! pos)
                g _ = Nothing
        
        -- Strings are evil inside case's and selectors
        f (Case (Const (AString x)) alts) = f (Case (expandStr1 x) alts)
        f (Sel (Const (AString x)) path) = f (Sel (expandStr1 x) path)
        
        f (Make ":" [Const (AChar x), Const (AString xs)]) = Const $ AString (x:xs)
        f (Make ":" [Const (AChar x), Make "[]" []]) = Const $ AString [x]
        
        f (Case on alts) | not (null ctrs) && complete = Case on (filter f alts)
            where
                f (Default _) = False
                f _ = True
            
                complete = ctrs `setEq` (map ctorName $ ctors $ getCtor hill $ head ctrs)
                ctrs = [c | AltCtr c _ <- alts]
        
        f x = x



---------------------------------------------------------------------


normalise :: ManipulateHill hill => hill -> hill
normalise hill = mapOverHill f hill
    where
        f (Apply x xs) = mkApply x xs
        f (Let xs x) = mkLet xs x
        f (Lambda n x) = mkLambda n x
        f x = x


applyFuns :: ManipulateHill hill => hill -> hill
applyFuns hill = mapOverHill f hill
    where
        f (Fun x) = Apply (Fun x) []
        f (Apply (Apply (Fun x) []) xs) = Apply (Fun x) xs
        f x = x


---------------------------------------------------------------------


useVector hill = mapOverHill f hill
    where
        f (Apply (Fun x) xs)
                | length xs >= nargs
                = mkApply (Call x use) extra
            where
                (use,extra) = splitAt nargs xs
                nargs = length $ funcArgs $ getFunc hill x
        
        f (Fun x)
                | null $ funcArgs $ getFunc hill x
                = Call x []
        
        f (Apply (Ctr x) xs)
                | length (ctorArgs ctor) == length xs
                = Make x xs
            where ctor = getCtor hill x
        
        f (Ctr x)
                | null $ ctorArgs $ getCtor hill x
                = Make x []

        f x = x


useVectorMake :: ManipulateHill hill => hill -> hill
useVectorMake x = mapOverHill f x
    where
        f (Apply (Ctr x) xs) = Make x xs
        f x = x


noVector hill = mapOverHill f hill
    where
        f (Call x xs) = Apply (Fun x) xs
        f (Make x xs) = Apply (Ctr x) xs
        f x = x


---------------------------------------------------------------------

-- do not use Integer, flip to Int everywhere
useInt :: Hill -> Hill
useInt x = mapOverHill f x
    where
        f (Case on alts) = Case on (map fa alts)
        f (Const x) = Const (fc x)
        f (Prim x xs) = Prim (primIntToInteger x) xs
        f x = x
        
        fa (AltConst x y) = AltConst (fc x) y
        fa x = x
        
        fc (AInteger x) = AInt $ fromInteger x
        fc x = x



varRejoin :: Hill -> Hill
varRejoin hill = mapOverHill f hill
    where
        -- collapse : @1.hd @1.tl ==> @1
        f (Make x ys@(Sel var _:_)) | f cs ys = var
            where
                f [] [] = True
                f (c:cs) (Sel a b:xs) = b == c && a == var && f cs xs
                f _ _ = False
            
                cs = ctorArgs $ getCtor hill x
        
        f x = x
