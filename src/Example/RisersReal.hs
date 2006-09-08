
module RisersReal where

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
readInt xs = f 0 xs
    where
        f acc (x:xs) = f ((acc * 10) + digitToInt x) xs
        f acc [] = acc


showInt :: Int -> String
showInt x | x < 10 = [intToDigit x]
          | otherwise = showInt (x `div` 10) ++ showInt (x `mod` 10)


intToDigit :: Int -> Char
intToDigit x = chr (ord '0' + x)

digitToInt :: Char -> Int
digitToInt x = ord x - ord '0'


ord x = fromEnum x
chr x = toEnum x
