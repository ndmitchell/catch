
module General.CmdLine2(
    Flag(..),
    parseCmdLine, unparseCmdLine,
    helpMsgShort, helpMsgLong
    ) where

import System.Console.GetOpt

data Flag = -- General Options
            Version | Help
            -- Standard Options
          | Quiet | Timeout Int | Skip [Int]
            -- Debug Options
          | DLog | DCore | DProfile | DTime | DMemory
            deriving (Eq, Show)

options = optionsNormal ++ optionsDebug

optionsNormal =
 [Option "v"  ["version"] (NoArg Version)            "show version number"
 ,Option "h?" ["help"]    (NoArg Help)               "show help message"
 ,Option "q"  ["quiet"]   (NoArg Quiet)              "final results only"
 ,Option "t"  ["timeout"] (OptArg timeout "SECONDS") "timeout per error, 0=inf"
 ,Option "s"  ["skip"]    (ReqArg skip "N,M")        "list of errors to skip"
 ]
 where
    timeout = Timeout . maybe 0 read
    skip = Skip . map read . words . map f
    f x = if x == ',' then ' ' else x

optionsDebug =
 [Option "" ["dlog"]     (NoArg DLog)     "ouptput property/precondition logs"
 ,Option "" ["dcore"]    (NoArg DCore)    "output intermediate Yhc.Core files"
 ,Option "" ["dtime"]    (NoArg DTime)    "display CPU time"
 ,Option "" ["dmemory"]  (NoArg DMemory)  "display memory useage"
 ,Option "" ["dprofile"] (NoArg DProfile) "run with profiling"
 ]


helpMsgShort = do
    putStrLn "Catch 2007, (C) Neil Mitchell 2005-2007, University of York\n"
    putStrLn "  catch [OPTION...] files...\n"
    putStr $ usageInfo "Standard options:" optionsNormal

helpMsgLong = do
    helpMsgShort
    putStrLn "\nDebug options:"
    putStr $ unlines $ drop (length optionsNormal + 1) $ lines
           $ usageInfo "" (optionsNormal ++ optionsDebug)


parseCmdLine = undefined

unparseCmdLine = undefined
