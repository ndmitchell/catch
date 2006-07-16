
module Typey.Reason(
    Reason(..), reasonSubtype,
    ReasonH, reasonInit, reasonTerm, reasonShow,
    reasonSection, reasonSubsection, reasonSubsubsection
    ) where

import Hite
import IO
import Typey.Subtype
import General.General
import Data.List
import Data.Char


data Reason = ReasonArgs [(FuncArg, TSubtype)] Reason
            | ReasonUnion TSubtype [Reason]
            | ReasonLookup TSubtype String
            | ReasonFollow TSubtype String Reason
            | ReasonApply TSubtype Reason Reason
            deriving Show


reasonSubtype :: Reason -> TSubtype
reasonSubtype (ReasonArgs _ x) = reasonSubtype x
reasonSubtype (ReasonUnion x _) = x
reasonSubtype (ReasonLookup x _) = x
reasonSubtype (ReasonFollow x _ _) = x
reasonSubtype (ReasonApply x _ _) = x


initial = unlines 
    ["<html>"
    ,"<head>"
    ,"<style type='text/css'>"
    ,"table {border-collapse: collapse;}"
    ,"td {text-align: center;}"
    ,"tr {vertical-align:bottom;}"
    ,".var tr, .func tr {vertical-align:middle;}"
    ,".overline {border-top:1px solid black;}"
    ,".bind {border-left: 1px solid red; border-right: 1px solid red;}"
    ,".bindtop {border-top: 1px solid red;}"
    ,".func {border-left: 1px solid blue; border-right: 1px solid blue;}"
    ,".functop {border-top: 1px solid blue;}"
    ,"sub {font-size: x-small;}"
    ,"body {font-family: sans-serif;}"
    ,"</style>"
    ,"</head>"
    ,"<body>"
    ]

final = unlines
    ["</body>"
    ,"</html>"
    ]



type ReasonH = Handle

reasonInit :: FilePath -> IO ReasonH
reasonInit s = do r <- openFile s WriteMode
                  hPutStrLn r initial
                  return r

reasonTerm :: ReasonH -> IO ()
reasonTerm r = do hPutStrLn r final
                  hClose r

reasonSection :: ReasonH -> String -> IO ()
reasonSection r = reasonDo r 1

reasonSubsection :: ReasonH -> String -> IO ()
reasonSubsection r = reasonDo r 2

reasonSubsubsection :: ReasonH -> String -> IO ()
reasonSubsubsection r = reasonDo r 3

reasonDo :: ReasonH -> Int -> String -> IO ()
reasonDo r n s = hPutStrLn r $ "<" ++ tag ++ s ++ "</" ++ tag
    where tag = "h" ++ show n ++ ">"

   
reasonShow :: ReasonH -> Reason -> IO ()
reasonShow r reason = hPutStrLn r (htmlReason reason)




htmlReason :: Reason -> String
htmlReason (ReasonArgs xs r) =
    concat ["<table class='var'><tr><td>" ++ a ++ " = </td><td>" ++ htmlSubtype b ++ "</td></tr></table><br/>\n" | (a,b) <- xs] ++
    htmlReason r
    
htmlReason (ReasonUnion r [x]) = htmlReason x

htmlReason (ReasonUnion r xs) = 
    intercat "<br/>UNION<br/>" (map htmlReason xs) ++
    "<br/>EQUALS<br/>" ++ htmlSubtype r

htmlReason (ReasonLookup t x) =
    "<table><tr><td>" ++ htmlSubtype t ++ "</td></tr>" ++
    "<tr><td class='overline'>" ++ x ++ "</td></tr></table>"

htmlReason (ReasonApply res lhs rhs) =
    "<table><tr><td>" ++ htmlReason lhs ++ "</td><td>" ++ htmlReason rhs ++ "</td></tr>" ++
    "<tr><td colspan='2' class='overline'>" ++ htmlSubtype res ++ "</td></tr></table>"

htmlReason (ReasonFollow res x y) =
    "<table><tr><td>" ++ htmlReason y ++ "</td><td>." ++ x ++ "</td></tr>" ++
    "<tr><td colspan='2' class='overline'>" ++ htmlSubtype res ++ "</td></tr></table>"

htmlReason x = show x


htmlSubtype :: TSubtype -> String
htmlSubtype (TFree xs) = intercat "'" $ map f xs
    where
        f x = a ++ "<sub>" ++ show b ++ "</sub>"
            where (a,b) = splitVar x
        

htmlSubtype (TBind xs) = htmlTable "bind" (map htmlPair xs)
htmlSubtype (TFunc xs) = htmlTable "func" (map htmlArr xs)
htmlSubtype TBot = "&perp;"
htmlSubtype TVoid = "*"
htmlSubtype TAny = "?"

htmlPair :: TPair -> [String]
htmlPair (TPair x y) = intercat "'" x : map htmlSubtype y

htmlArr :: TArr -> [String]
htmlArr (TArr x y) = intersperse "&rarr;" $ map htmlSubtype x ++ [htmlSubtype y]


htmlTable :: String -> [[String]] -> String
htmlTable cls [] = "<table class='" ++ cls ++ "'><tr><td><i>none</i></td></tr></table>"
htmlTable cls (c:ells) = "<table class='" ++ cls ++ "'>" ++ f False c ++ concatMap (f True) ells ++ "</table>"
    where
        f border row = tr ++ "<td>" ++ intercat "</td><td>" row ++ "</td></tr>"
            where tr = if border then "<tr class='" ++ cls ++ "top'>" else "<tr>"
        