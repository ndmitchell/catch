
module Reqs.Type(
    Req(..), Reqs, ReqAll(..), ReqAlls, reqsNot
    ) where

import Pred.Type
import Pred.Show
import Reqs.Path

import Hite
import General.General
import Char
import List


data Req = Req {reqExpr :: Expr, reqPath :: Path String, reqCtors :: [CtorName]}
           deriving Eq
           
data ReqAll = ReqAll {reqForall :: FuncName, reqWithin :: Reqs}
              deriving Eq


type Reqs = Pred Req
type ReqAlls = Pred ReqAll


instance Blur Req where
    blur req = req{reqPath = pathBlur (reqPath req)}

instance Blur ReqAll where
    blur (ReqAll on within) = ReqAll on (blur within)

instance (Eq a, Blur a) => Blur (Pred a) where
    blur x = mapPredLit (predLit . blur) x


reqsNot :: Hite -> Reqs -> Reqs
reqsNot hite x = predNot f x
    where
        f (Req on path set) = predLit $ Req on path (getCtorsFromCtor hite (head set) \\ set) 
