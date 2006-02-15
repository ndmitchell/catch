
module Core.Show(showCore, showCoreExpr) where

import List
import Core.Type

showCore = showPretty
showCoreExpr = unlines . showExpr



-- stuff below copied from the Yhc compiler
-- Core.Pretty
showPretty :: Core -> String
showPretty x = unlines $ showCore2 x

indent :: [String] -> [String]
indent = map ("    " ++)


showCore2 :: Core -> [String]
showCore2 (Core xs) = if null res then [] else tail res
    where res = concat $ map (([] :) . showItem) xs

showItem :: CoreItem -> [String]
showItem (CoreData name ctors) =
    ("data " ++ name ++ " =") :
    (indent $ map showCtor ctors)

showItem (CoreFunc decl body) =
    (noBracket (showExprLine decl) ++ " = ") :
    (indent $ showExpr body)


showCtor :: CoreCtor -> String
showCtor (CoreCtor name args) = concat $ intersperse " " (name : map f args)
    where
        f Nothing = "_"
        f (Just x) = x


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
showExpr (CoreInteger x) = [show x]

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

showExpr (CoreLet x y) = ["let"] ++ indent (showCore2 $ Core x) ++ ["in"] ++ showExpr y


singleton [x] = True
singleton _ = False

