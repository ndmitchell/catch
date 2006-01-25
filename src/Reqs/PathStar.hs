
module Reqs.PathStar(
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
    pathSubset
    ) where

import Star.Type
import Star.Show
import Star.Blur

type Path a = Star a


pathIntegrate a b = starCon [starLit a, b]

pathQuotient a b = quotient a b

pathBlur x = blur x

pathLambda :: (Eq a, Show a) => Star a
pathLambda = Lambda

pathIsEmpty x = x == Omega

pathIsEwp x = isEwp x

pathIsSingle x = isSingle x

pathUnion a b = starUni [a, b]

pathIntersect a b = starInt [a, b]

pathReverse a = starRev a

pathIsOmega x = x == Omega

pathSubset a b = starSubset a b
