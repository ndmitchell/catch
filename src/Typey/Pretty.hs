
module Typey.Pretty(typeyPretty) where

import Hite
import Typey.Type


typeyPretty :: FilePath -> [(FuncName, [Subtype], Subtype)] -> IO ()
typeyPretty file items = writeFile (file ++ ".htm") res
    where
        res = prefix ++ concatMap showItem items ++ suffix


prefix = "<html><body>"
suffix = "</body></html>"


showItem :: (FuncName, [Subtype], Subtype) -> String
showItem (name, args, res) =
    "<table><tr>" ++
    "<td>" ++ name ++ " ::</td>" ++
    concat ["<td>" ++ showSubtype a ++ " -&gt;</td>" | a <- args] ++
    "<td>" ++ showSubtype res ++ "</td>"


showSubtype :: Subtype -> String
showSubtype x = show x
