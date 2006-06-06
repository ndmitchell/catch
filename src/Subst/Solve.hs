
module Subst.Solve(substSolve) where

import qualified Data.Map as Map
import qualified Data.IntMap as IntMap
import Data.Predicate
import Data.List
import Data.Maybe

import Subst.Type
import Subst.Show
import Hite


substSolve :: Hite -> Env -> IO Bool
substSolve hite x = f 10 x
    where
        f 0 x = putStrLn "Gave up" >> return False
        f n x = print x2 >> f (n-1) x2
            where x2 = pass hite x


pass :: Hite -> Env -> Env
pass hite env@(Env subst bind) = env3
    where
        env3 = Env (IntMap.map f subst2) bind2
            where
                f x = mapSExp g x
                g x@(SFunc name args) = case Map.lookup (name,args) bind2 of
                                           Just a -> SVar a
                                           Nothing -> x
                g x = x
    
        (Env subst2 bind2) = foldr (addFunc hite) env reqFuncs
    
        reqFuncs = [func | x <- IntMap.elems subst, func@(SFunc name args) <- allSExp x,
                           all isConcrete args, not (Map.member (name,args) bind)]


addFunc :: Hite -> SExp -> Env -> Env
addFunc hite (SFunc name args) (Env subst bind) =
        Env (IntMap.insert new res3 subst) (Map.insert (name,args) new bind)
    where
        new = IntMap.size subst
        Func _ params (MCase opts) _ = getFunc hite name
        
        res3 = if name == "error" then SBot else res2
        res2 = case res of
                   [] -> SFree -- this means impossible, is Free the right choice?
                   [x] -> x
                   xs -> SChoice xs
                   
        res = [replace x | MCaseAlt p x <- opts, isConsistent p]

        isConsistent p = demandBool $ mapPredLit f p
            where
                f (MCaseOpt var ctor) = case getVar var of
                                            Just SFree -> predTrue
                                            Just (SCtor n _) -> predBool $ n == ctor
                                            _ -> predFalse
                
        getVar :: Expr -> Maybe SExp
        getVar (Var n) = Just $ args !! (fromJust $ elemIndex n params)
        getVar (Sel x s) = case getVar x of
                               Just SFree -> Just SFree
                               Just (SCtor n a) -> case elemIndex s $ ctorArgs (getCtor hite n) of
                                                        Just i -> Just (a !! i)
                                                        Nothing -> Nothing
                               _ -> Nothing

        replace x = case x of
                        x | isSel x || isVar x -> fromJust $ getVar x
                        Call (CallFunc name) args -> SFunc name (map replace args)
                        Make name args -> SCtor name (map replace args)
                        Msg _ -> SFree
                        _ -> error $ "Subst.Solve.replace, " ++ show x
        