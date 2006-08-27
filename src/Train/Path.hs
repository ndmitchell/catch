
module Train.Path(Path, nullPath, newPath, ewpPath, emptyPath, integrate, differentiate) where

import General.General
import Data.Char
import Hite
import Control.Monad


data Path = Path [PathElem]
			deriving (Eq, Show, Ord)


data PathElem = PathAtom CtorArg
	          | PathStar CtorArg
	          deriving (Eq, Show, Ord)


instance Output Path where
	output (Path xs) = concatMap (('.':) . map toUpper . output) xs

instance Output PathElem where
	output (PathAtom x) = x
	output (PathStar x) = x ++ "*"


isPathStar (PathStar{}) = True ; isPathStar _ = False
isPathAtom = not . isPathStar


nullPath (Path x) = null x

emptyPath = Path []

newPath xs = foldl integrate emptyPath xs


ewpPath (Path x) = all isPathStar x


isStar x = x `elem` ["tl"]


differentiate :: Path -> CtorArg -> Maybe Path
differentiate (Path xs) ctor = liftM Path $ f xs
	where
		f [] = Nothing
		f (PathAtom x:xs) | x == ctor = Just xs
						  | otherwise = Nothing
		f (PathStar x:xs) | x == ctor = Just (PathStar x:xs)
						  | otherwise = f xs
						  


integrate :: Path -> CtorArg -> Path
integrate (Path x) ctor = Path (f x)
	where
		f ys@(PathStar y:_) | ctor == y = ys
		f ys = (if isStar ctor then PathStar ctor else PathAtom ctor) : ys



