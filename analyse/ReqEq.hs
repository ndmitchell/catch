
module ReqEq(
    equalReq, equalReqs,
    enumerateReqs
    ) where

import Req
import Path
import PathCtor
import PathCtorEq
import Data.List
import Data.Maybe
import Data.Proposition
import Yhc.Core
import General
import Debug.Trace



equalReq :: Req -> Req -> Bool
equalReq (Req a1 b1@(PathCtor core _ _)) (Req a2 b2) = a1 == a2 && equalPathCtor core b1 b2
equalReq a b = a == b


equalReqs :: Reqs -> Reqs -> Bool
equalReqs a b = res -- trace (show (a,b,normalise core $ enumerateReqs a, normalise core $ enumerateReqs b, res)) res
    where
        res = equalValue core (enumerateReqs a) (enumerateReqs b)
        PathCtor core _ _ = reqPath $ head $ propAll a ++ propAll b



enumerateReqs :: Reqs -> [Val]
enumerateReqs x = enumeratePathCtorProp $ (propFold folder x :: PropSimple PathCtor)
    where
        folder = PropFold {foldOr = propOrs, foldAnd = propAnds, foldNot = propNot, foldLit = f}
    
        PathCtor core _ _ = reqPath $ head $ propAll x
        core2 = core{coreDatas = newData : coreDatas core}
        newData = CoreData "#" [] [CoreCtor "#" [("",Just ('#' : show i)) | i <- [0..arity-1]]]
        
        f (Req a (PathCtor _ (Path path) ctors)) = propLit $ PathCtor core2 (Path (p:path)) ctors
            where p = PathAtom $ '#' : (show $ fromJust $ findIndex (== a) vars)
        
        arity = length vars
        vars = snub $ map reqExpr $ propAll x

