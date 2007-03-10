
module General.CmdLine(
    Stage(..), Option(..),
    parseCommandLine, helpMessage
    ) where

import Data.Char
import Data.List


data Stage = Compile | Overlay | Firstify | LetElim | Analyse
             deriving (Show, Enum, Bounded, Eq, Ord)

data Option = Text | Html | Yca | NoLog
              deriving (Show, Enum, Bounded, Eq, Ord)


allStages = [minBound..maxBound] :: [Stage]


parseCommandItem :: String -> ([Stage],[Option])
parseCommandItem x
        | x == "all" = (allStages, [])
        | otherwise = case (f x, f x) of
                           ([x],[]) -> ([x],[])
                           ([],[x]) -> ([],[x])
                           _ -> error $ "Unknown command line flag, " ++ x
    where
        f x = [y | y <- [minBound..maxBound], map toLower (show y) == x]


parseCommandLine :: [String] -> ([String], [Stage], [Option])
parseCommandLine xs = (files, let s = f a in if null s then allStages else s, f b)
    where
        f x = sort $ nub $ concat x
        (a,b) = unzip $ map (parseCommandItem . tail) flags
        (flags, files) = partition ("-" `isPrefixOf`) xs



helpMessage = putStr $ unlines
    ["Catch 2007, let's have fun!"
    ,"(C) Neil Mitchell 2005-2007, University of York"
    ,""
    ,"  catch [stages] [options] [files]"
    ,""
    ,"stages:  -all" ++ concat [", -" ++ map toLower (show x) | x <- [minBound..maxBound] :: [Stage]]
    ,"options: -" ++ concat (intersperse ", -" [map toLower (show x) | x <- [minBound..maxBound] :: [Option]])
    ]
