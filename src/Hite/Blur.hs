
module Hite.Blur(blurExpr, unrollExpr) where

import Hite.Type
import Options


unrollExpr :: Expr -> Expr
unrollExpr r@(Repeat expr alt) = 
    case expr of
        Make x xs -> Make x (map f xs)
        Call x xs -> Call x (map f xs)
        Sel  x y  -> Sel  (f x) y
    where
        f RepeatNow = r
        f x = x



blurExpr :: Expr -> Expr
blurExpr x = mapExpr f x
    where
        f = blurToExpr . blurBlur . exprToBlur


data Mode = ModeSel | ModeMake | ModeCall
            deriving Eq


data Blur = Raw Expr
          | Rep Mode String [Blur]
          deriving Eq



exprToBlur :: Expr -> Blur
exprToBlur (Call (CallFunc x) xs) = Rep ModeCall x (map exprToBlur xs)
exprToBlur (Make x xs) = Rep ModeMake x (map exprToBlur xs)
exprToBlur (Sel x y) = Rep ModeSel y [exprToBlur x]
exprToBlur x = Raw x


blurToExpr :: Blur -> Expr
blurToExpr (Raw x) = x
blurToExpr (Rep mode name xs) =
    case mode of
        ModeSel  -> Sel (blurToExpr $ head xs) name
        ModeMake -> Make name (map blurToExpr xs)
        ModeCall -> Call (CallFunc name) (map blurToExpr xs)


blurBlur :: Blur -> Blur
blurBlur (Raw x) = Raw x
blurBlur r@(Rep m n xs)
        | not (null repeats) = xs !! head repeats
        | null depths = r
        | otherwise   = Raw $ Repeat expr alt
    where
        expr = blurToExpr $ Rep m n (replace pos xs (Raw RepeatNow))
        alt  = blurToExpr $ drill (depth-hiteBlurTo) r
        poss = [0 .. length xs - 1]
    
        (pos, depth) = head depths -- maybe max on this one?
        depths = [(pos, depth) | pos <- poss, depth <- [blurDepth r pos], depth >= hiteBlurFrom]

        drill 0 r = r
        drill d (Rep m n xs) = drill (d-1) (xs !! pos)
        
        repeats = [pos | pos <- poss, isRepeat r pos]

        ignore n xs = take n xs ++ drop (n+1) xs
        replace n xs rep = take n xs ++ [rep] ++ drop (n+1) xs

    

    
blurDepth :: Blur -> Int -> Int
blurDepth (Rep m n xs) pos = 
    case xs !! pos of
        r@(Rep m2 n2 xs2)
            | m2 == m && n2 == n && length xs2 == length xs
            -> 1 + blurDepth r pos
        _ -> 0


isRepeat :: Blur -> Int -> Bool
isRepeat (Rep m n xs) pos =
    case xs !! pos of
        Raw (Repeat expr alt) | m == m2 && n == n2 && length xs == length xs2 &&
                                (xs2 !! pos) == Raw RepeatNow && all f (zip xs xs2)
                              -> True
                where
                    (Rep m2 n2 xs2) = exprToBlur expr

                    f (a, Raw RepeatNow) = True
                    f (a, b) = a == b
        _ -> False
