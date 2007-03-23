-- #CATCH {: _ * : {False},[]} | {[]}

-- some versions got this wrong in an unsafe way!

module BitCount where

num                  :: [Bool] -> Int
num []               =  0
num (a:as)           =  fromEnum a + num as

main sel          =  assert (num sel <= 1)

assert True = True
