
module Preamble where

import Prelude hiding (
    -- standard
    map, head, (.), reverse
    )


-- SPECIAL FUNCTIONS

-- yhc bottoms out if some of these are not imported

preamble_undefined = preamble_undefined

_apply1 f x = f x


preamble_error_empty_list = []
preamble_error msg = case preamble_error_empty_list of
    (_:_) -> undefined


prim_STRING x = x
prim_str = undefined


-- STANDARD FUNCTIONS
head (x:xs) = x

map f [] = []
map f (x:xs) = f x : map f xs


reverse xs = reverse_acc xs []
    where
        reverse_acc [] ys = ys
        reverse_acc (x:xs) ys = reverse_acc xs (x:ys)
