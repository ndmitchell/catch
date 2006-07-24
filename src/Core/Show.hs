
module Core.Show(showCore, showCoreExpr) where

import Core.Type
import Core.Pretty

showCore = showPretty
showCoreExpr = unlines . showExpr True


