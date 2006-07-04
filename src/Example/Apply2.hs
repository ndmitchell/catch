
module Apply2 where

main = apply id False True

apply f x y = f x && f y
