
module Core.Show(showCore) where

import List
import Core.Type


showCore :: Core -> String
showCore x = unlines $ showLines x

indent :: [String] -> [String]
indent = map ("    " ++)


showLines :: Core -> [String]
showLines (Core xs) = concat $ intersperse [[]] $ map showFunc xs

showFunc :: CoreFunc -> [String]
showFunc (CoreFunc decl body) =
    (noBracket (showExprLine decl) ++ " = ") :
    (indent $ showExpr body)


showExprLine :: CoreExpr -> String
showExprLine y = case showExpr y of
        [x] -> x
        xs -> "{" ++ concat (intersperse "; " xs) ++ "}"

noBracket ('(':xs) = init xs
noBracket x = x


showExpr :: CoreExpr -> [String]
showExpr (CoreCon x) = [x]
showExpr (CoreVar x) = [x]
showExpr (CoreInt x) = [show x]
showExpr (CoreChr x) = [show x]
showExpr (CoreStr x) = [show x]
showExpr (CorePos x y) = showExpr y

showExpr (CoreApp x y) = if all singleton items
        then ["(" ++ concat (intersperse " " (map head items)) ++ ")"]
        else ["("] ++ indent (concat items) ++ [")"]
    where
        items = map showExpr (x:y)

showExpr (CoreCase x y) = line1 ++ rest
    where
        line1 = if singleton subject
                then ["case " ++ head subject ++ " of"]
                else ["case"] ++ indent subject ++ ["of"]
        
        subject = showExpr x
        rest = concatMap f y
        
        f (a,b) = indent $ [noBracket (showExprLine a) ++ " ->"] ++ indent (showExpr b)

showExpr (CoreLet x y) = ["let"] ++ indent (showLines $ Core x) ++ ["in"] ++ showExpr y


singleton [x] = True
singleton _ = False
