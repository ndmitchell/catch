
module Graph.Rename(Rename, getRename, validRename, mergeRename, applyRename) where

import Graph.Type
import Hite
import Data.Predicate
import General.General

import Control.Exception
import Data.List
import Data.Maybe
import Debug.Trace


data Rename = Rename [(String, GExp)]
            | RenameInvalid


getRename :: Hite -> Func -> GExp -> Pred MCaseOpt -> Rename
getRename hite func (GCtor "." args) p 
        | length args /= length (funcArgs func) = error "Graph.Rename.getRename, args length mismatch"
        | isTrue p = Rename []
        | all isLit ps = mergeRename $ map (f . fromLit) ps
        | otherwise = error "Graph.Rename.getRename: Can't handle predOr's"
    where
        ps = fromAnd p
        
        
        f :: MCaseOpt -> Rename
        f opt@(MCaseOpt s ctor) = getRenameOne hite (args !! argNo) opt
            where
                argNo = fromJust $ elemIndex var (funcArgs func)
                
                var = f s
                f (Var a) = a
                f (Sel a b) = f a

{-                
        

        -- generate an initial expression, with its name
        generate :: MCaseOpt -> (String, GExp)
        generate (MCaseOpt s ctor) = (var,
                GCtor ctor [GVar $ var ++ "." ++ x | x <- ctorArgs $ getCtor hite ctor])
            where var = output s

        solve :: GExp -> [(String, GExp)] -> GExp
        solve b [] = b
        solve b (x:xs) = solve (merge x b) $ map (\(a,b) -> (a,merge x b)) xs

        merge :: (String, GExp) -> GExp -> GExp
        merge (name,rep) x = mapGExp f x
            where
                f (GVar n) | n == name = rep
                f x = x
-}


-- assume that the root of GExp is the variable refered to by MCaseOpt
-- can ignore the Var at the room of the MCaseOpt
getRenameOne :: Hite -> GExp -> MCaseOpt -> Rename
getRenameOne hite expr (MCaseOpt on ctor) = mergeRename $ map (demand expr) unroll

    where
        demand x ([],ctr) = case x of
                                (GVar a) -> Rename [(a, newCtor ctr a)]
                                (GCtor a b) -> if a == ctr then Rename [] else RenameInvalid
                                _ -> RenameInvalid
        demand x (y:ys, ctr) = case x of
                                (GVar a) -> demand (GVar (a ++ "." ++ y)) (ys, ctr)
                                (GCtor a b) -> case elemIndex y (ctorArgs $ getCtor hite a) of
                                                    Just n -> demand (b !! n) (ys,ctr)
                                                    Nothing -> RenameInvalid
                                _ -> RenameInvalid
    
        -- the complete path from the expression to the ctor
        path = f on
            where
                f (Var _) = []
                f (Sel x y) = f x ++ [y]
    
        -- all the demands, in order
        unroll = (map f $ tail $ inits path) ++ [(path,ctor)]
            where
                f x = (pth, ctorName $ getCtorFromArg hite res)
                    where (pth,res) = (init x, last x)

        newCtor :: CtorName -> String -> GExp
        newCtor ctor var = GCtor ctor [GVar $ var ++ "." ++ x | x <- ctorArgs $ getCtor hite ctor]


mergeRename :: [Rename] -> Rename
mergeRename [] = Rename []
mergeRename [x] = x
mergeRename (Rename a:Rename b:xs) = mergeRename (Rename (a++b) : xs)
mergeRename _ = RenameInvalid


validRename :: Rename -> Bool
validRename RenameInvalid = False
validRename (Rename xs) = all isValid (groupSetBy cmp xs)
    where
        cmp a b = fst a == fst b
        isValid (x:xs) = all (x ==) xs


applyRename :: Rename -> GExp -> GExp
applyRename ren x = assert (validRename ren) $ mapGExp f x
    where
        Rename rep = ren
    
        f (GVar x) = case lookup x rep of
                        Just y -> y
                        Nothing -> GVar x
        f x = x
