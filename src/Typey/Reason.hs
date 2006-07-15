
module Typey.Reason(
    Reason(..), reasonSubtype,
    ReasonH, reasonInit, reasonTerm, reasonShow,
    reasonSection, reasonSubsection, reasonSubsubsection
    ) where

import Hite
import IO
import Typey.Subtype


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


type ReasonH = Handle

reasonInit :: FilePath -> IO ReasonH
reasonInit s = openFile s WriteMode

reasonTerm :: ReasonH -> IO ()
reasonTerm r = hClose r

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
reasonShow r reason = hPutStrLn r (show reason)
