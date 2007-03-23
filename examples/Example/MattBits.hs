
module MattBits where

num                  :: [Bool] -> Int
num []               =  0
num (a:as)           =  fromEnum a + 2 * num as

main sel xs          =  xs !! num sel



{-

num                  :: [Bool] -> Int
num []               =  0
num (a:as)           =  bit2int a + num as

main sel xs          =  assert (num xs < 0)


assert True = True


bit2int False        =  0
bit2int True         =  1
-}
