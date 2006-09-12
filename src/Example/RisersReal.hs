
module RisersReal where

main :: IO ()
main = do
    c <- getContents 
    putStrLn $ show $ length $ risers $ takeWhile (/= 0) $ map read $ lines c


risers :: Ord a => [a] -> [[a]]
risers [] = []
risers [x] = [[x]]
risers (x:y:etc) = if x <= y then (x:s):ss else [x]:(s:ss)
   where (s:ss) = risers (y:etc)

