
module ZipInit where

main x y = zipinit x y

zipinit [] ys = ys
zipinit (x:xs) (y:ys) = (x : y) : zipinit xs ys

