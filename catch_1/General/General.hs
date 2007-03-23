
module General.General(module General.General, trace) where

import Data.List
import Control.Monad
import System.Environment
import System.IO
import Debug.Trace


type Result a = (Bool,a)

success = (,) True
failure = (,) False


snub :: Ord a => [a] -> [a]
snub = map head . group . sort 


traceStrict msg res | length msg >= 0 = trace msg res

traceS msg res = trace (show msg) res

traceThis res = traceS res res


groupKey :: Ord key => [(key,val)] -> [(key,[val])]
groupKey = map f . groupBy ((==) `on` fst) . sortBy (compare `on` fst)
    where f xs = (fst $ head xs, map snd xs)


ungroupKey :: [(key,[val])] -> [(key,val)]
ungroupKey = concatMap (\(k,v) -> map ((,) k) v)


on :: (new -> new -> res) -> (orig -> new) -> (orig -> orig -> res)
on f g x y = f (g x) (g y)


concatMapM f xs = liftM concat $ mapM f xs



twoDp :: Float -> String
twoDp x = pre ++ "." ++ post
    where
        x2 = round (x * 100)
        s = show x2
        ss = replicate (3 - length s) '0' ++ s
        (pre,post) = splitAt (length ss - 2) ss 


readFileStrict :: FilePath -> IO String
readFileStrict file = do
    h <- openFile file ReadMode
    src <- hGetContents h
    length src `seq` hClose h
    return src


baseDir :: IO FilePath
baseDir = catch (getEnv "CATCH_BASE_PATH")
                (\_ -> return "..")
