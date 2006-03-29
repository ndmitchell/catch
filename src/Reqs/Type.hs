
module Reqs.Type(
    Req(..), Reqs, blurReqsPath, allForall
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
         | ReqEnv {reqExpr :: Expr, reqPath :: Path String, reqCtors :: [CtorName], reqIn :: Expr}
           deriving Eq


type Reqs = Pred Req


blurReqPath :: Req -> Req
blurReqPath (ReqAll on within) = ReqAll on (blurReqsPath within)
blurReqPath req = req{reqPath = pathBlur (reqPath req)}

blurReqsPath :: Reqs -> Reqs
blurReqsPath = mapPredLit (predLit . blurReqPath)



allForall :: Reqs -> [FuncName]
allForall req = [x | ReqAll x _ <- allPredLit req]
