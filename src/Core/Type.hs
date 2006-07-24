
module Core.Type(module Core.CoreType, module Core.Type) where

import Core.CoreType


isCoreData (CoreData {}) = True
isCoreData _ = False

isCoreFunc = not . isCoreData


class PlayCore a where
    mapCore :: (CoreExpr -> CoreExpr) -> a -> a
    allCore :: a -> [CoreExpr]

    
instance PlayCore a => PlayCore [a] where
    mapCore f xs = map (mapCore f) xs
    allCore xs = concatMap allCore xs


instance PlayCore Core where
    mapCore f (Core n d x) = Core n d $ mapCore f x
    allCore (Core n d x) = allCore x


instance PlayCore CoreItem where
    mapCore f (CoreFunc x y) = CoreFunc (mapCore f x) (mapCore f y)
    mapCore f x = x
    
    allCore (CoreFunc x y) = allCore x ++ allCore y
    allCore x = []


instance PlayCore CoreExpr where
    mapCore f x = f $ case x of
        CoreApp a xs -> CoreApp (mapCore f a) (mapCore f xs)
        CoreCase a xs -> CoreCase (mapCore f a) (map (\(a,b) -> (mapCore f a, mapCore f b)) xs)
        CoreLet xs y -> CoreLet (mapCore f xs) (mapCore f y)
        CorePos x y -> CorePos x (mapCore f y)
        _ -> x

    allCore x = x : concatMap allCore (case x of
            CoreApp x ys -> x : ys
            CoreCase a xs -> a : concatMap (\(a,b) -> [a,b]) xs
            CoreLet xs ys -> allCore xs ++ allCore ys
            CorePos _ a -> [a]
            _ -> []
        )
