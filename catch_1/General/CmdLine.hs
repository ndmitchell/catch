
module General.CmdLine(
    Flag(..),
    parseCmdLine, unparseCmdLine,
    helpMsgShort, helpMsgLong,
    defaultTimeout
    ) where

import System.Console.GetOpt
import System.Exit
import System.Environment
import Control.Monad
import Data.List
import Data.Char
import Data.Version(showVersion)
import Paths_catch

data Flag = -- General Options
            Version | Help
            -- Standard Options
          | Quiet | Timeout Int | Skip [Int] | Yhc String
            -- Debug Options
          | DLog | DCore | DProfile | DTime | DMemory
            deriving (Eq, Show)

-- Turn off a timeout since it does not seem to work on GHC
defaultTimeout = 0 :: Int

options = optionsNormal ++ optionsDebug

optionsNormal =
 [Option "v"  ["version"] (NoArg Version)            "show version number"
 ,Option "h?" ["help"]    (NoArg Help)               "show help message"
 ,Option "q"  ["quiet"]   (NoArg Quiet)              "final results only"
 ,Option "t"  ["timeout"] (OptArg timeout "SECONDS") "timeout per error, 0=inf"
 ,Option "s"  ["skip"]    (ReqArg skip "N,M")        "list of errors to skip"
 ,Option "y"  ["yhc"]     (ReqArg Yhc "flags")       "extra flags to give to Yhc"
 ]
 where
    timeout = Timeout . maybe 0 (read2 "timeout")
    skip = Skip . map (read2 "skip") . words . map f
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


parseCmdLine :: IO ([Flag],[String])
parseCmdLine = do
        args <- getArgs
        let (flags,files,errs) = getOpt Permute options args

        when (not $ null errs) $ do
            putStrLn "Error in command line, see --help"
            putStr $ unlines errs
            exitFailure
        when (Version `elem` flags) $ do
            putStrLn $ "Catch 2007, version " ++ showVersion version
            exitSuccess
        when (Help `elem` flags) $ do
            helpMsgLong
            exitSuccess
        when (null files) $ do
            helpMsgShort
            exitSuccess

        return (flags,files)

exitSuccess = exitWith ExitSuccess


unparseCmdLine :: [Flag] -> [String] -> String
unparseCmdLine flags files = unwords $ map f flags ++ map g files
    where
        f (Timeout i) = "--timeout=" ++ show i
        f (Skip is) = "--skip=" ++ concat (intersperse "," $ map show is)
        f (Yhc s) = "\"--yhc=" ++ s ++ "\""
        f x = "--" ++ map toLower (show x)

        g x = "\"" ++ x ++ "\""


read2 msg x = case reads x of
                  [(y,"")] -> y
                  _ -> error $ "Could not parse " ++ msg ++ ", found: " ++ show x
