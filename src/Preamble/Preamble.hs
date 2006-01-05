
module Preamble where

preamble_undefined = preamble_undefined

_apply1 f x = f x


preamble_error_empty_list = []
preamble_error msg = case preamble_error_empty_list of
    (_:_) -> undefined


prim_STRING x = x
prim_str = undefined


