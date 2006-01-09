
module Core.Type where


data Core = Core [CoreFunc]
            deriving (Show, Read)


data CoreFunc = CoreFunc CoreExpr CoreExpr
                deriving (Show, Read, Eq)


data CoreExpr = CoreCon String
              | CoreVar String
              | CoreApp CoreExpr [CoreExpr]
              | CoreInt Int
              | CoreChr Char
              | CoreStr String
              | CoreCase CoreExpr [(CoreExpr,CoreExpr)]
              | CoreLet [CoreFunc] CoreExpr
              | CorePos String CoreExpr
                deriving (Show, Read, Eq)


class PlayCore a where
    mapCore :: (CoreExpr -> CoreExpr) -> a -> a
    allCore :: a -> [CoreExpr]

    
instance PlayCore a => PlayCore [a] where
    mapCore f xs = map (mapCore f) xs
    allCore xs = concatMap allCore xs


instance PlayCore Core where
    mapCore f (Core x) = Core $ mapCore f x
    allCore (Core x) = allCore x


instance PlayCore CoreFunc where
    mapCore f (CoreFunc x y) = CoreFunc (mapCore f x) (mapCore f y)
    allCore (CoreFunc x y) = allCore x ++ allCore y


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
