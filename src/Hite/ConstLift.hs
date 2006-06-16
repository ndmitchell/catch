
-- If a function is a constant then inline it
-- If a function only calls constant functions, then inline it

module Hite.ConstLift(cmd) where

import Hite.Type
import General.General
import Data.List
import Data.Maybe


cmd = cmdHitePure (const constLift) "const-lift"
            "Lift all constants"


constLift :: Hite -> Hite
constLift = resultConst . dropWrappers


resultConst :: Hite -> Hite
resultConst hite = fixp hite []
    where
        fixp hite done = if null res then hite else fixp (mapExpr (g res) hite) done2
            where
                done2 = map fst res ++ done
                res = concatMap isConst $ filter (\x -> not $ funcName x `elem` done) $ funcs hite
    
        g res x@(Call (CallFunc n) xs) =
            case lookup n res of
                Nothing -> x
                Just i -> i
        g res x = x

        isConst (Func name _ body _) = [(name,body) | all f $ allExpr body]
            where
                f (Make _ _) = True
                f (Msg _) = True
                f _ = False


-- if f x = x then drop all calls to f x with x
-- mainly targets primString etc
dropWrappers :: Hite -> Hite
dropWrappers hite = mapExpr g hite
    where
        g x@(Call (CallFunc n) xs) =
            case lookup n res of
                Nothing -> x
                Just i -> xs !! i
        g x = x

        res = concatMap f (funcs hite)
        
        f (Func name args (Var i) _) = [(name, fromJust $ elemIndex i args)]
        f _ = []
