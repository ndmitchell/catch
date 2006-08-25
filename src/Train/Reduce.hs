
module Train.Reduce(reduce) where

import Train.Type
import General.General


reduce :: Req -> Req
reduce req@(Req hite expr path ctors) = case expr of
	ZCall{} -> req
	ZVar{} -> req
	_ -> error $ "reduce: " ++ output req
