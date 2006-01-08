
module Pred.Type where

import List

-- * Data type definition
data Pred a = PredOr  [Pred a]
            | PredAnd [Pred a]
            | PredLit a


-- * Show instance
instance Show a => Show (Pred a) where
    show x = case x of
            PredOr  [] -> "False"
            PredAnd [] -> "True"
            PredLit  a -> show a
            PredOr  xs -> disp 'v' xs
            PredAnd xs -> disp '^' xs
        where
            disp sym xs = "(" ++ mid ++ ")"
                where mid = concat $ intersperse [' ',sym,' '] (map show xs)


-- * Play instances

-- ** Play on the predicate
mapPred :: (Pred a -> Pred a) -> Pred a -> Pred a
mapPred f x = f $ case x of
        PredOr  xs -> predOr  (fs xs)
        PredAnd xs -> predAnd (fs xs)
        x -> x
    where
        fs = map (mapPred f)


mapPredM :: Monad m => (Pred a -> m (Pred a)) -> Pred a -> m (Pred a)
mapPredM f x = do y <- case x of
                      PredOr xs  -> do ys <- fs xs
                                       return $ predOr  xs
                      PredAnd xs -> do ys <- fs xs
                                       return $ predAnd xs
                      x -> return x
                  f y
    where
        fs xs = mapM (mapPredM f) xs


allPred :: Pred a -> [Pred a]
allPred x = x : concatMap allPred (case x of
        PredAnd y -> y
        PredOr  y -> y
        _ -> []
        )

-- ** Play on the literal
mapPredLit :: (a -> Pred a) -> Pred a -> Pred a
mapPredLit f x = mapPred g x
    where
        g (PredLit a) = f a
        g a = a

mapPredLitM :: Monad m => (a -> m (Pred a)) -> Pred a -> m (Pred a)
mapPredLitM f x = mapPredM g x
    where
        g (PredLit a) = f a
        g a = return a
        
allPredLit x = concatMap f (allPred x)
    where
        f (PredLit x) = [x]
        f _ = []


-- * Construction and simplification

predFalse :: Pred a
predFalse = PredOr []


predTrue :: Pred a
predTrue = PredAnd []


predOr :: [Pred a] -> Pred a
predOr xs = case filter (not . isFalse) $ concatMap f xs of
                [x] -> x
                xs -> PredOr xs
    where
        f (PredOr x) = x
        f x = [x]

predAnd :: [Pred a] -> Pred a
predAnd xs = case filter (not . isTrue) $ concatMap f xs of
                 [x] -> x
                 xs -> PredAnd xs
    where
        f (PredAnd x) = x
        f x = [x]
        

predLit :: a -> Pred a
predLit x = PredLit x



reducePred :: Pred a -> Pred a
reducePred x = mapPred f x
    where
        f (PredAnd x) = predAnd x
        f (PredOr  x) = predOr  x
        f (PredLit x) = predLit x


isFalse (PredOr  []) = True
isFalse (PredAnd xs) = any isFalse xs
isFalse (PredOr  xs) = all isFalse xs
isFalse _ = False

isTrue (PredAnd []) = True
isTrue (PredAnd xs) = all isTrue xs
isTrue (PredOr  xs) = any isTrue xs
isTrue _ = False
