
module Dictionary where


main :: Int -> Int -> String -> String -> Bool
main a b c d = test a b && same c d


test :: Ord a => a -> a -> Bool
test a b = a == b && a < b

same :: Eq a => a -> a -> Bool
same a b = a == b
