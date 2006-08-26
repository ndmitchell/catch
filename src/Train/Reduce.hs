
module Train.Reduce(reduce, reduces, reduceWithM, reducesWithM) where

import Train.Type
import General.General
import Data.Predicate


reduces :: Reqs -> Reqs
reduces reqs = mapPredLit reduce reqs


reduce :: Req -> Reqs
reduce req@(Req hite expr path ctors) = case expr of
	ZCall{} -> predLit req
	ZVar{} -> predLit req
	_ -> reduces $ reduceOne req


-- apply 1 step reduction to a Sel or a Make
-- this function does the real work!
reduceOne :: Req -> Reqs
reduceOne req@(Req hite expr path ctors) = case expr of
	ZSel x y -> predLit $ Req hite x (path `integrate` y) ctors
	ZMake y xs | nullPath path -> predBool $ y `elem` ctors
	_ -> error $ "reduceOne: " ++ output req
	


-- take a function that reduces a Call
-- and reduce the entire thing
reducesWithM :: Monad m => (Req -> m Reqs) -> Reqs -> m Reqs
reducesWithM f reqs = mapPredLitM (reduceWithM f) reqs


reduceWithM :: Monad m => (Req -> m Reqs) -> Req -> m Reqs
reduceWithM f req@(Req hite expr path ctors) = case expr of
	ZCall{} -> f req >>= reducesWithM f
	ZVar{} -> return $ predLit req
	_ -> reducesWithM f $ reduceOne req
	