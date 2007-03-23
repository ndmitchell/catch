
module MattSort where

evens f []         = []
evens f (a:b:cs)   = f [a,b] ++ evens f cs

riffle ([],[])     = []
riffle ([a],[])    = [a]
riffle (a:as,b:bs) = a : b : riffle (as,bs)

unriffle []        = ([],[])
unriffle [a]       = ([a],[])
unriffle (a:b:cs)  = (a:as,b:bs)
  where
  (as,bs)          = unriffle cs

two f (a,b)        = (f a,f b)

ilv f              = riffle . two f . unriffle

bfly 1 f           = f
bfly n f           = evens f . ilv (bfly (n-1) f)

parl f g as        = f bs ++ g cs
  where
    n              = (length as + 1) `div` 2
    (bs,cs)        = splitAt n as

sortB 0 f          = id
sortB n f          = bfly n f
                       . parl (sortB (n-1) f) (reverse . sortB (n-1) f)

cmp                :: [Bool] -> [Bool]
cmp [a,b]          =  if a < b then [a,b] else [b,a]

main               :: Int -> [Bool] -> [Bool]
main n x            =  sortB n cmp x


{-



                                             
module MattSort where


parl f g as        = []


sortB n = 
    let res = id (parl (sortB n) (sortB n))
    in
        case n == n of
            True -> id
            False -> res

main               :: Int -> [Bool] -> [Bool]
main n x            =  sortB n x
-}
