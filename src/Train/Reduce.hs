
module Train.Reduce(reduce) where

import Train.Type
import General.General
import Data.Predicate


reduce :: Req -> Reqs
reduce req@(Req hite expr path ctors) = case expr of
	ZCall{} -> predLit $ req
	ZVar{} -> predLit $ req
	ZSel x y -> reduce (Req hite x (path `integrate` y) ctors)
	_ -> error $ "reduce: " ++ output req
