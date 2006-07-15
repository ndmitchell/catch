
module Typey.Reason(
    Reason(..), reasonSubtype,
    ReasonH, reasonInit, reasonTerm, reasonShow,
    reasonSection, reasonSubsection, reasonSubsubsection
    ) where

import Hite
import IO
import Typey.Subtype
import General.General


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
    ,"td {text-align: center;}"
    ,"tr {vertical-align:bottom;}"
    ,".overline {border-top:1px solid black;}"
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
htmlReason (ReasonArgs xs r) = concat [a ++ " := " ++ htmlSubtype b ++ "<br/>\n" | (a,b) <- xs] ++ htmlReason r
htmlReason (ReasonUnion r [x]) = htmlReason x

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
htmlSubtype x = output x
