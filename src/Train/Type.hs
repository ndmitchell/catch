
module Train.Type(module Train.Path, module Train.Type, module Train.Req) where

import Hite
import General.General
import Train.Path
import Train.Req


data ZHite = ZHite [Data] [ZFunc]


getZFunc :: ZHite -> FuncName -> ZFunc
getZFunc (ZHite _ funcs) name = head [res | res@(ZFunc nam _ _) <- funcs, nam == name]


data ZFunc = ZFunc FuncName [FuncArg] [(Reqs, Either String ZExpr)]


instance Manipulate ZExpr where
	getChildren x = case x of
		ZCall _ x -> x
		ZMake _ x -> x
		ZSel x _ -> [x]
		_ -> []

	setChildren x ys = case x of
		ZCall x _ -> ZCall x ys
		ZMake x _ -> ZMake x ys
		ZSel _ x -> let [y] = ys in ZSel y x
		_ -> x

