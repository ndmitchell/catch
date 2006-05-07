
module Reqs.Type(
    Req(..), Reqs, blurReqsPath, allForall, reqsNot
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


blurReqPath :: Req -> Req
blurReqPath (ReqAll on within) = ReqAll on (blurReqsPath within)
blurReqPath req = req{reqPath = pathBlur (reqPath req)}

blurReqsPath :: Reqs -> Reqs
blurReqsPath = mapPredLit (predLit . blurReqPath)



allForall :: Reqs -> [FuncName]
allForall req = [x | ReqAll x _ <- allPredLit req]



reqsNot :: Hite -> Reqs -> Reqs
reqsNot hite x = predNot f x
    where
        f (Req on path set) = predLit $ Req on path (getCtorsFromCtor hite (head set) \\ set) 
