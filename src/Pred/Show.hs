
module Pred.Show where

import Pred.Type
import List
import General.General

instance Show a => Show (Pred a) where
    show x = showPred x
    

showPred :: Show a => Pred a -> String
showPred x = showPredBy show x

prettyPred :: Show a => Pred a -> String
prettyPred x = prettyPredBy show x


showPredBy :: (a -> String) -> Pred a -> String
showPredBy f x =
    case x of
        PredOr  [] -> "False"
        PredAnd [] -> "True"
        PredLit  a -> f a
        PredOr  xs -> disp 'v' xs
        PredAnd xs -> disp '^' xs
    where
        disp sym xs = "(" ++ mid ++ ")"
            where mid = concat $ intersperse [' ',sym,' '] $ map (showPredBy f) xs


prettyPredBy :: (a -> String) -> Pred a -> String
prettyPredBy showItem x = unlines $ f x
    where
        f x = case x of
            PredOr  [] -> ["False"]
            PredAnd [] -> ["True"]
            PredLit  a -> [showItem a]
            PredOr  xs -> g 'v' xs
            PredAnd xs -> g '^' xs

        g sym xs | all isAtom xs = ["(" ++ (concat $ intersperse [' ',sym,' '] $ map (head . f) xs) ++ ")"]
                 | otherwise = ["("] ++ (indents $ concat $ intersperse [[sym]] $ map f xs) ++ [")"]
        
        isAtom (PredOr  []) = True
        isAtom (PredAnd []) = True
        isAtom (PredLit _ ) = True
        isAtom _ = False
