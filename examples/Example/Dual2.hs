
module Dual2 where


main = assert (eq True True && eq False False) True

assert True x = x

eq True True = True
eq False False = True
eq _ _ = False
