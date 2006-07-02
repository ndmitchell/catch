
module Typey.Permute(permuteTypes) where

import Typey.Type
import Typey.Subtype
import General.General

import Data.List


-- permute the types then rename them
permuteTypes :: DataM SmallT -> [Large2T] -> [[TSubtype]]
permuteTypes datam x = map uniqueFrees $ getSubtypesList datam x




extractFrees :: [TSubtype] -> [TSubtype]
extractFrees x = concatMap fSubtype x
    where
        fSubtype (TFree a) = [TFree a]
        fSubtype (TBind a) = concatMap fPair a
        fSubtype (TFunc a) = concatMap fArr a
        fSubtype (TBot) = []
        
        fArr (TArr a b) = concatMap fSubtype a ++ fSubtype b
        fPair (TPair a b) = concatMap fSubtype b


replaceFrees :: [TSubtype] -> [TSubtype] -> [TSubtype]
replaceFrees x ns = fSubtypes x ns
    where
        fSubtypes :: [TSubtype] -> [TSubtype] -> [TSubtype]
        fSubtypes [] [] = []
        fSubtypes (x:xs) ns = fSubtype x n1 : fSubtypes xs n2
            where (n1,n2) = splitAt (length $ extractFrees [x]) ns
        fSubtypes x y = error $ show ("fSubtypes",x,y)

            
        fSubtype (TFree a) [n] = n
        fSubtype (TBot) [] = TBot
        fSubtype (TFunc [TArr a b]) n = TFunc [TArr (init ab) (last ab)]
            where ab = fSubtypes (a ++ [b]) n
        fSubtype (TFunc as) ns = TFunc $ map (\(TFunc [a]) -> a) $ fSubtypes (map (TFunc . box) as) ns
        fSubtype (TBind xs) n = TBind $ fPairs xs n

        fPairs [] [] = []
        fPairs (TPair a b : xs) ns = TPair a (fSubtypes b n1) : fPairs xs n2
            where (n1,n2) = splitAt (length $ extractFrees b) ns


uniqueFrees :: [TSubtype] -> [TSubtype]
uniqueFrees x = replaceFrees x $ f [] $ extractFrees x
    where
        f rep [] = []
        f rep (TFree [a]:xs) = TFree [a ++ show n] : f ((a,n+1):rep) xs
            where
                n = case lookup a rep of
                        Nothing -> 0
                        Just x -> x
        f rep (x:xs) = x : f rep xs


getSubtypesList :: DataM SmallT -> [Large2T] -> [[TSubtype]]
getSubtypesList datam xs = crossProduct $ map (getSubtypes datam) xs


getSubtypes :: DataM SmallT -> Large2T -> [TSubtype]
getSubtypes datam (Free2T x) = [TFree [x]]
getSubtypes datam (Ctor2T x) = getSubtypes datam (Bind2T (Ctor2T x) [])
getSubtypes datam (Arr2T xs ys) = box $ TFunc [TArr x y | x <- getSubtypesList datam xs, y <- TBot : getSubtypes datam ys]
getSubtypes datam (Bind2T (Ctor2T x) y) = concatMap f typs
    where
        typs = typePermute $ lookupJust x datam
        
        f (TBind xs) = map TBind $ crossProduct $ map g xs
        
        g :: TPair -> [TPair]
        g (TPair x y) = map (TPair x) $ crossProduct $ map h y
        
        h :: TSubtype -> [TSubtype]
        h (TFree []) = [TFree []]
        h (TFree [s]) = getSubtypes datam (y !! read s)
        
        


-- generate all possible permutations
-- use TFree to stand for all the free variables
typePermute :: DataT SmallT -> [TSubtype]
typePermute (DataT n xs) =
        [TBind [x]| x <- nper] ++ [TBind [x,gs y] | x <- rper, y <- powerSet (rper++nper), not (null y)]
    where
        (rper,nper) = (map f rctr, map f nctr)
        (rctr,nctr) = partition isRecursive xs
        
        f (CtorT name xs) = TPair [name] $ map g [0..n-1]
            where
                g i = if i `elem` frees then TFree [show i] else TFree []
                frees = nub [i | FreeS i <- xs]

        gs = foldr1 g
        g (TPair a1 b1) (TPair a2 b2) = TPair (a1 `union` a2) (zipWith h b1 b2)
            where
                h (TFree x) (TFree y) = TFree (x `union` y)
