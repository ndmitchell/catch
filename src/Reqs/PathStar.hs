
module Reqs.PathStar(
    Path,
    pathIntegrate,
    pathQuotient,
    pathBlur,
    pathLambda,
    pathIsEmpty,
    pathIsEwp,
    pathIsSingle
    ) where

import Star.Type
import Star.Show
import Star.Blur

type Path a = Star a


pathIntegrate a b = starCon [starLit a, b]

pathQuotient x = quotient x

pathBlur x = blur x

pathLambda :: (Eq a, Show a) => Star a
pathLambda = Lambda

pathIsEmpty x = x == Omega

pathIsEwp x = isEwp x

pathIsSingle x = isSingle x
