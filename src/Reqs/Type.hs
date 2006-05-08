
module Reqs.Type(
    Req(..), Reqs, reqsNot
    ) where

import Pred.Type
import Pred.Show
import Reqs.Path

import Hite
import General.General
import Char
import List


data Req = Req {reqExpr :: Expr, reqPath :: Path String, reqCtors :: [CtorName]}
         | ReqAll {reqForall :: FuncName, reqWithin :: Reqs}
           deriving Eq


type Reqs = Pred Req


instance Blur Req where
    blur (ReqAll on within) = ReqAll on (blur within)
    blur req = req{reqPath = pathBlur (reqPath req)}


instance (Eq a, Blur a) => Blur (Pred a) where
    blur x = mapPredLit (predLit . blur) x


reqsNot :: Hite -> Reqs -> Reqs
reqsNot hite x = predNot f x
    where
        f (Req on path set) = predLit $ Req on path (getCtorsFromCtor hite (head set) \\ set) 
