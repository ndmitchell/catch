
module Typey.Pretty(typeyPretty) where

import Hite
import Typey.Type
import Data.List


typeyPretty :: FilePath -> [(FuncName, [Subtype], Subtype)] -> IO ()
typeyPretty file items = writeFile (file ++ ".htm") res
    where
        res = prefix ++ concatMap showItem items ++ suffix


prefix = "<html><head><style type='text/css'>\n" ++ unlines style ++ "\n</style></head><body>"
suffix = "</body></html>"

style =
    [".tb,.t,.b {background-color: #ddd; text-align:center;}"
    ,".t {border-bottom:0;}"
    ,".b {border-top:0;}"
    ,".o1 {font-size: 100%;}"
    ,".o2 {font-size: 200%;}"
    ]

showItem :: (FuncName, [Subtype], Subtype) -> String
showItem (name, args, res) =
    "<table><tr>" ++
    "<td>" ++ name ++ " ::</td>" ++
    concat ["<td>" ++ nice a ++ "</td><td>&rarr;</td>" | a <- args] ++
    "<td>" ++ nice res ++ "</td>" ++
    "</tr></table>\n"


class Nice a where
    nice :: a -> String
    nices :: [a] -> String
    
    nices = concatMap nice


instance Nice Subtype where
    nice (Atom x) = nices x
    nice (Subtype (a :@ b) ([] :@ d)) = 
        "<table><tr><td class='tb'>" ++ nices a ++ "</td><td class='o1'>(</td><td>" ++
        nices b ++ "</td><td class='o1'>)</td></tr></table>"
    nice s@(Subtype (a :@ b) (c :@ d)) =
        "<table cellspacing='0'><tr><td class='t'>" ++ nices a ++
        "</td><td rowspan='2' class='o2'>(</td>" ++
        "<td>" ++ nices b ++ "</td><td rowspan='2' class='o2'>)</td></tr>" ++
        "<tr><td class='b'>" ++ nices c ++ "</td><td>" ++ nices d ++ "</td></tr></table>"


instance Nice Subvalue where
    nices [] = "?"
    nices [x] = nice x
    nices xs = "[" ++ (concat $ intersperse "," $ map nice xs) ++ "]"

    nice Bot = "&perp;"
    nice (SVar n) = "#" ++ show n
    nice (SCtor s) = s
