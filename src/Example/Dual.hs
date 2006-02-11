
module Dual where


main x = dual x

dual x = assert (x `eq` x) True

assert True x = x


eq True True = True
eq False False = True
eq _ _ = False
