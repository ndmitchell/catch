
module Reqs.Path(
    Path,
    pathIntegrate,
    pathQuotient,
    pathBlur,
    pathLambda,
    pathIsEmpty,
    pathIsEwp
    ) where

import RegExp.Type
import RegExp.Parse
import RegExp.Prop
import RegExp.Blur
import RegExp.Simplify


type Path a = RegExp a


pathIntegrate a b = regConcat [regLit a, b]

pathQuotient x = quotient x

pathBlur x = blur x

pathLambda :: (Eq a, Show a) => Path a
pathLambda = regLambda

pathIsEmpty x = isEmpty x

pathIsEwp x = isEwp x
