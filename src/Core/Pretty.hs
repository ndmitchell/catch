-- FROM YHC
-- SLIGHTLY RENAMED

module Core.Pretty(showPretty, showExpr) where

import List
import Maybe
import Char
import Core.CoreType


dropModule :: String -> String
dropModule x = f x False x
    where
        f x False ('.':_) = x
        f _ True  ('.':x) = f x False x
        f x _ (_:xs) = f x True xs
        f x _ [] = x

isOperator x = case dropModule x of
                   (x:_) | isAlphaNum x || x `elem` "'_" -> False
                   _ -> True


showPretty :: Core -> String
showPretty x = unlines $ showCore x

indent :: [String] -> [String]
indent = map ("    " ++)


showCore :: Core -> [String]
showCore (Core modName depends xs) =
    ("module " ++ modName ++ " where") : "" :
    map ("import " ++) depends ++ "" :
    showItems xs

showItems :: [CoreItem] -> [String]
showItems xs = concat $ intersperse [[]] $ map showItem xs

showItem :: CoreItem -> [String]
showItem (CoreData name free []) = ["data " ++ name ++ concatMap (' ':) free]
showItem (CoreData name free (c:tors)) =
    ("data " ++ name ++ concatMap (' ':) free ++ " =") :
    ("      " ++ showCtor c) :
    (indent $ map (("| " ++) . showCtor) tors)

showItem (CoreFunc decl body) =
    (showExprLine False decl ++ " = ") :
    (indent $ showExpr False body)


showCtor :: CoreCtor -> String
showCtor (CoreCtor name args) = name ++ " " ++
        ['{' | useRecords] ++
        (concat $ intersperse sep $ map f args) ++
        ['}' | useRecords]
    where
        useRecords = any (isJust . snd) args
        sep = ([','|useRecords]++" ")
        
        f (typ, Nothing) = typ
        f (typ, Just x) = "_" ++ x ++ " :: " ++ typ


showExprLine :: Bool -> CoreExpr -> String
showExprLine b y = case showExpr b y of
        [x] -> x
        xs -> "{" ++ concat (intersperse "; " xs) ++ "}"


bracket False x = x
bracket b [x] = ["(" ++ x ++ ")"]
bracket b xs = ["("] ++ indent xs ++ [")"]


-- True = should bracket
showExpr :: Bool -> CoreExpr -> [String]
showExpr b (CoreCon x) = showExpr b (CoreVar x)
showExpr b (CoreVar x) | x == "Prelude.[]" = ["[]"] -- technically these aren't in
                       | x == "Prelude.:" = ["(:)"] -- the prelude
                       | isOperator x = ["(" ++ x ++ ")"]
                       | otherwise = [x]
showExpr b (CoreInt x) = [show x]
showExpr b (CoreChr x) = [show x]
showExpr b (CoreStr x) = [show x]
showExpr b (CorePos x y) = showExpr b y
showExpr b (CoreInteger x) = [show x]

showExpr b (CoreApp x []) = showExpr b x
showExpr b (CoreApp x y) = bracket b $
        if all singleton items
        then [concat (intersperse " " (map head items))]
        else concat items
    where
        items = map (showExpr True) (x:y)

showExpr b (CoreCase x y) = bracket b $ line1 ++ rest
    where
        line1 = if singleton subject
                then ["case " ++ head subject ++ " of"]
                else ["case"] ++ indent subject ++ ["of"]
        
        subject = showExpr True x
        rest = concatMap f y
        
        f (a,b) = indent $ [showExprLine False a ++ " ->"] ++ indent (showExpr False b)

showExpr b (CoreLet x y) = bracket b $ ["let"] ++ indent (showItems x) ++ ["in"] ++ showExpr True y


singleton [x] = True
singleton _ = False
