
module Typey.Pretty(typeyPretty) where

import Hite
import Typey.Type


typeyPretty :: FilePath -> [(FuncName, [Subtype], Subtype)] -> IO ()
typeyPretty file items = writeFile (file ++ ".htm") res
    where
        res = prefix ++ concatMap showItem items ++ suffix


prefix = "<html><head><style type='text/css'>\n" ++ unlines style ++ "\n</style></head><body>"
suffix = "</body></html>"

style =
    ["table {border:1px solid black;}"
    ,"td {border:1px solid red;}"
    ,".tb,.t,.b {border:1px solid blue;}"
    ,".t {border-bottom:0;}"
    ,".b {border-top:0;}"
    ,".o {font-size: 2em;}"
    ]

showItem :: (FuncName, [Subtype], Subtype) -> String
showItem (name, args, res) =
    "<table><tr>" ++
    "<td>" ++ name ++ " ::</td>" ++
    concat ["<td>" ++ nice a ++ "</td><td>-&gt;</td>" | a <- args] ++
    "<td>" ++ nice res ++ "</td>" ++
    "</tr></table>\n"


class Nice a where
    nice :: a -> String
    nices :: [a] -> String
    
    nices = concatMap nice


instance Nice Subtype where
    nice (Atom x) = show (Atom x)
    nice (Subtype (a :@ b) ([] :@ d)) = 
        "<table><tr><td class='tb'>" ++ nices a ++ "</td><td class='o'>(</td><td>" ++
        nices b ++ "</td><td class='o'>)</td></tr></table>"
    nice (Subtype (a :@ b) (c :@ d)) =
        "<table><tr><td class='t'>" ++ nices a ++ "</td><td rowspan='2'>(</td>" ++
        "<td>" ++ nices b ++ "</td><td rowspan='2'>)</td></tr>" ++
        "<tr><td class='b'>" ++ nices c ++ "</td><td>" ++ nices d ++ "</td></tr></table>"

instance Nice Subvalue where
    nices x = show x
    nice x = show x

