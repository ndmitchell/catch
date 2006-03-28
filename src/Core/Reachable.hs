
module Core.Reachable(coreReachable) where

import Core.Type

import qualified Data.Set as Set
import qualified Data.Map as Map
import List

coreReachable :: Core -> Core
coreReachable (Core n x) = Core n (f True Set.empty Set.empty Map.empty x)
    where
        -- f False    []   xs = filter isCoreData xs

        f wantMain want done pending [] = 
            if wantMain then
                error "coreReachable, main was not found"
            else
                []
        
        f wantMain want done pending (x:xs) =
                if isCoreData x then
                    x : f wantMain want done pending xs
                else if wantMain && isMain then
                    addItem False want done pending [x]
                else if name `Set.member` want then
                    addItem wantMain (Set.delete name want) done pending [x]
                else
                    f wantMain want done (Map.insert name x pending) xs
            where
                isMain = "niam." `isPrefixOf` reverse name
                (CoreFunc (CoreApp (CoreVar name) _) _) = x
                
                addItem wantMain want done pending [] = f wantMain want done pending xs
                addItem wantMain want done pending (add:rest) = add : newRest ++ addItem wantMain newWant newDone pending rest
                    where
                        newRest = map ((Map.!) pending) inPending
                        newWant = want `Set.union` Set.fromDistinctAscList outPending
                        newDone = name `Set.insert` done
                        newPending = remFromMap pending inPending
                    
                        (inPending, outPending) = partition (`Map.member` pending) realDeps
                        realDeps = filter (not . (`Set.member` done)) deps
                        deps = map head $ group $ sort [i | CoreVar i <- allCore body]
                        (CoreFunc  (CoreApp (CoreVar name) _) body) = add


remFromMap :: Ord k => Map.Map k v -> [k] -> Map.Map k v
remFromMap x [] = x
remFromMap x (v:vs) = remFromMap (v `Map.delete` x) vs

