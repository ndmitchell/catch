
module General where

import Maybe
import Directory
import List

{-
import IOExts

debugmsg msg val = unsafePerformIO $
                        do putStrLn (show msg)
                           return val

debugmsgres msg val = debugmsg (msg, val) val
-}

a ! b = fromJust $ lookup b a

errorS a = error (show a)

fsts = map fst
snds = map snd

filterFst f = filter (f . fst)
filterSnd f = filter (f . snd)

lookupDef def val lst = case lookup val lst of
                            Nothing -> def
                            Just x -> x


readFileMaybe :: FilePath -> IO (Maybe String)
readFileMaybe file = do x <- doesFileExist file
                        if x
                            then do y <- readFile file
                                    return (Just y)
                            else return Nothing


eqUnordered :: Ord a => [a] -> [a] -> Bool
eqUnordered xs ys = sort xs == sort ys


strSet :: [String] -> String
strSet xs = "{" ++ concat (intersperse "," xs) ++ "}"


splitEither :: [Either a b] -> ([a], [b])
splitEither (Left  x:xs) = let (a,b) = splitEither xs in (x:a, b)
splitEither (Right x:xs) = let (a,b) = splitEither xs in (a, x:b)
splitEither [] = ([], [])

