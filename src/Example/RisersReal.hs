
module RisersReal where

import Data.Char

main :: IO ()
main = do
    c <- getContents 
    putStrLn $ showInt $ length $ risers $ takeWhile (/= 0) $ map readInt $ lines c


risers :: Ord a => [a] -> [[a]]
risers [] = []
risers [x] = [[x]]
risers (x:y:etc) = if x <= y then (x:s):ss else [x]:(s:ss)
   where (s:ss) = risers (y:etc)



readInt :: String -> Int
readInt x = f 1 $ reverse x
    where
        f base (x:xs) = (digitToInt x * base) + (f (base*10) xs)
        f base [] = 0

showInt :: Int -> String
showInt x | x < 10 = [intToDigit x]
          | otherwise = showInt (x `div` 10) ++ [intToDigit (x `mod` 10)]
