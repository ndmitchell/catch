
module Typey.Abstract where

import Typey.Type
import General.General
import Hite


data Abstract = Bit Bool
              | List [Abstract]
              | AbsBottom
              | AbsVoid

b0 = Bit False
b1 = Bit True


instance Show Abstract where
    show (Bit x) = if x then "1" else "0"
    show (AbsBottom) = "!"
    show (AbsVoid) = "#"
    show (List xs) = "[" ++ concatMap show xs ++ "]"



unionAbs :: [Abstract] -> Abstract
unionAbs xs = foldr f AbsVoid xs
    where
        f AbsVoid x = x
        f x AbsVoid = x
        f AbsBottom (List (x:xs)) = List (b1:xs)
        f (List (x:xs)) AbsBottom = List (b1:xs)
        f (Bit i) (Bit j) = Bit (i && j)
        f (List i) (List j) = List $ zipWithEq f i j




-- List [b0,b0,b1,List[b0],b1,b1,List[b0]]

getAbstract :: DataM SmallT -> Large2T -> Abstract
getAbstract datam x = f x
    where
        f (Free2T _) = List [b0]
        f (Arr2T _ _) = error "getAbstract: Cannot abstract for a higher order function"
        f (Bind2T (Ctor2T ctor) args) = List $ b0 : lst ++ rlst
            where
                rlst = if any isRecursive ctrs then lst else []
                lst = replicate (length ctrs) b1 ++ map f args
                (DataT n ctrs) = lookupJust ctor datam


{-
f x
    where
        f (Free2T x)
        f (Ctor2T 


        data Large2T = Ctor2T String
                     | Free2T String
                     | Bind2T Large2T [Large2T]
             | Arr2T [Large2T] Large2T
-}


hasCtorAbs :: DataM SmallT -> Abstract -> CtorName -> Bool
hasCtorAbs _ (List [_,_,Bit a,_,_,_,_]) ":" = a
hasCtorAbs _ (List [_,Bit a,_,_,_,_,_]) "[]" = a


followSelAbs :: DataM SmallT -> Abstract -> String -> Abstract
followSelAbs _ (List [_,_,_,a,_,_,_]) "hd" = a
followSelAbs _ (List [b,_,_,_,n,c,a]) "hd" = List [b,n,c,a,n,c,a]
