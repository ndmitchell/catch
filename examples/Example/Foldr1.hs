
module Foldr1 where

catch_any = catch_any

main x = foldr1 f x
    where
        f a b = if catch_any then a else b