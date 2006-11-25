
module Tram.Path(Path, nullPath, newPath, ewpPath, pathCtorArgs,
	emptyPath, finitePath, makeFinitePath, integrate, differentiate,
	subsetPath, blurPath) where

import General.General
import Data.Char
import Data.List
import Hill.All
import Control.Monad


data Path = Path Hill [PathElem]

instance Eq Path where
	(Path _ a) == (Path _ b) = a == b

instance Ord Path where
	compare (Path _ a) (Path _ b) = compare (f a, a) (f b, b)
		where
			f xs = (length atm, 0 - length str, 0 - length (concat [x | PathStar x <- xs]))
				where (str,atm) = partition isPathStar xs

data PathElem = PathAtom CtorArg
	          | PathStar [CtorArg]
	          deriving (Eq, Ord)


instance Show Path where
	show (Path _ xs) = concatMap (('.':) . map toUpper . show) xs

instance Show PathElem where
	show (PathAtom x) = x
	show (PathStar [x]) = x ++ "*"
	show (PathStar xs) = "(" ++ intercat "+" xs ++ ")*"


isPathStar (PathStar{}) = True ; isPathStar _ = False
isPathAtom = not . isPathStar


pathCtorArgs (Path _ x) = snub $ concatMap f x
    where
        f (PathAtom x) = [x]
        f (PathStar xs) = xs


nullPath (Path _ x) = null x

emptyPath hite = Path hite []

newPath hite xs = foldl integrate (emptyPath hite) xs


ewpPath (Path _ x) = all isPathStar x

finitePath (Path _ x) = all (not . isPathStar) x
makeFinitePath (Path hite x) = Path hite $ filter (not . isPathStar) x



differentiate :: Path -> CtorArg -> Maybe Path
differentiate (Path hite xs) ctor = liftM (Path hite) $ f xs
	where
		f [] = Nothing
		f (PathAtom x:xs) | ctor == x = Just xs
						  | otherwise = Nothing
		f (PathStar x:xs) | ctor `elem` x = Just (PathStar x:xs)
						  | otherwise = f xs
						  

isStar hite x = TyCon (dataName obj) (map TyFree $ frees obj) == cargType obj
	where obj = getCArg hite x


integrate :: Path -> CtorArg -> Path
integrate (Path hite x) ctor = Path hite (PathAtom ctor : x) -- -} (f x)
	where
		carg = getCArg hite ctor
		star = TyCon (dataName carg) (map TyFree $ frees carg) == cargType carg
	
		f (PathStar y:ys) 
			| star && dataName (getCArg hite $ head y) == dataName carg
			= PathStar (sort $ nub $ ctor:y) : ys
		f ys = (if star then PathStar [ctor] else PathAtom ctor) : ys




-- is the first a subset of the second
subsetPath :: Path -> Path -> Bool
subsetPath (Path _ a) (Path _ b) = f a b
	where
		f (PathAtom x:xs) (PathAtom y:ys) | x == y = f xs ys
		f (PathStar x:xs) (PathStar y:ys) | x `subset` y = f xs ys
		f xs              (PathStar y:ys) = f xs ys
		f [] [] = True
		f _ _ = False


-- blur paths are required
-- the result must be a superset of the input
blurPath :: Hill -> Path -> Path
blurPath hill (Path hite x) = Path hite (combineSucc $ map useStar x)
    where
        useStar (PathAtom ctor)
            | TyCon (dataName carg) (map TyFree $ frees carg) == cargType carg
            = PathStar [ctor]
            where carg = getCArg hite ctor
        useStar x = x
    
    
        combineSucc (PathStar x1:PathStar x2:xs)
            | dataName (getCArg hite $ head x1) == dataName (getCArg hite $ head x2)
            = combineSucc (PathStar (snub $ x1 ++ x2) : xs)
        combineSucc (x:xs) = x : combineSucc xs
        combineSucc [] = []
