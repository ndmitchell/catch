
module Prepare.ShortCtors(shortCtors) where

import Yhc.Core


shortCtors :: Core -> Core
shortCtors = mapUnderCore shortExpr . applyCtorCore (\x -> x{coreCtorName = short (coreCtorName x)})


shortExpr :: CoreExpr -> CoreExpr
shortExpr (CoreCon x) = CoreCon $ short x
shortExpr x = x


short :: CoreCtorName -> CoreCtorName
short = reverse . takeWhile (/= '.') . reverse
