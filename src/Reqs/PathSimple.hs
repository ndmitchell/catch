
module Reqs.PathSimple(
    Path,
    pathIntegrate,
    pathQuotient,
    pathBlur,
    pathLambda,
    pathIsEmpty,
    pathIsEwp,
    pathIsSingle,
    pathUnion,
    pathIntersect,
    pathReverse,
    pathIsOmega,
    pathIsLambda,
    pathSubset,
    pathPretty,
    pathIsFinite,
    pathMakeFinite,
    pathEnumerate
    ) where

import General.General


data Path a = PathOmega
            | Path [PathLit a]
            deriving Eq

data PathLit a = PathStar a
               | PathLit a
               deriving Eq


isInf x = x `elem` ["tl"]

isPathStar (PathStar _) = True
isPathStar _ = False

isPathLit = not . isPathStar

isPathOmega PathOmega = True
isPathOmega _ = False

mkPath xs = Path $ f xs
    where
        f (PathStar x:PathStar y:xs) | x == y = f (PathStar x:xs)
        f (x:xs) = x : f xs
        f [] = []


pathIntegrate a PathOmega = PathOmega
pathIntegrate a (Path xs) | isInf a = mkPath (PathStar a : xs)
                          | otherwise = mkPath (PathLit a : xs)

pathQuotient a PathOmega = PathOmega
pathQuotient a (Path []) = PathOmega
pathQuotient a (Path (PathLit b:xs)) | a == b = Path xs
                                     | a /= b = PathOmega
pathQuotient a (Path (PathStar b:xs)) | a == b = Path (PathStar b:xs)
                                      | a /= b = pathQuotient a (Path xs)


pathBlur x = x


pathLambda = Path []

pathIsEmpty PathOmega = True
pathIsEmpty _ = False

pathIsEwp (Path xs) | all isPathStar xs = True
pathIsEwp _ = False

pathIsSingle x = error "todo 1" -- isSingle x

pathUnion a b | a `pathSubset` b = Just b
              | b `pathSubset` a = Just a
              | otherwise = Nothing

pathIntersect a b = error "todo 3" -- starInt [a, b]

pathReverse a = error "todo 4" -- starRev a

pathIsOmega x = error "todo 5" -- x == Omega

pathIsLambda x = error "todo 6" -- x == Lambda

pathSubset a b | isPathOmega a || isPathOmega b = True
pathSubset xs ys = pathMakeFinite xs == pathMakeFinite ys


-- error "todo 7" -- starSubset a b

pathPretty PathOmega = "0"
pathPretty (Path []) = "1"
pathPretty (Path xs) = intercat "." $ map f xs
    where
        f (PathLit a) = a
        f (PathStar a) = a ++ "*"


pathIsFinite PathOmega = False
pathIsFinite (Path xs) = all isPathLit xs


pathMakeFinite PathOmega = PathOmega
pathMakeFinite (Path xs) = Path (filter isPathLit xs)

pathEnumerate (Path xs) = [[x | PathLit x <- xs]]
