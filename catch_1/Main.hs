
module Main where

import System.Environment
import System.FilePath
import System.Directory
import Control.Monad
import Data.Char
import Data.List
import Prepare.All
import Yhc.Core


data Stage = Compile | Overlay | Firstify | LetElim | Analyse
             deriving (Show, Enum, Bounded, Eq, Ord)

data Option = Text | Html
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



main :: IO ()
main = do
    xs <- getArgs
    let (files,stages,options) = parseCommandLine xs
    if null files then do
        putStr $ unlines
            ["Catch 2007, let's have fun!"
            ,"(C) Neil Mitchell 2005-2007, University of York"
            ,""
            ,"  catch [stages] [options] [files]"
            ,""
            ,"stages:  -all" ++ concat [", -" ++ map toLower (show x) | x <- [minBound..maxBound] :: [Stage]]
            ,"options: -" ++ concat (intersperse ", -" [map toLower (show x) | x <- [minBound..maxBound] :: [Option]])
            ]
     else
        mapM_ (execFile stages options) files


execFile :: [Stage] -> [Option] -> String -> IO ()
execFile stages options file = do
        realfile <- findFileHs file
        let yca     = dropFileName realfile </> "ycr" </> replaceExtension (takeFileName realfile) "yca"
            over    = replaceExtension yca "over.yca"
            first   = replaceExtension yca "first.yca"
            letelim = replaceExtension yca "letelim.yca"

        putStrLn $ "Executing: " ++ file ++ ", " ++ realfile
        when (Compile `elem` stages) $ compile realfile
        res <- test Overlay  over (Right yca) (liftM wrap . overlay)
        res <- test Firstify first res (return . firstify)
        res <- test LetElim  letelim res (return . wrap . letElim)

        return ()
    where
        wrap x = ("",x)
    
        test :: Stage -> FilePath -> Either Core FilePath -> (Core -> IO (String,Core)) -> IO (Either Core FilePath)
        test x out inp f | x `notElem` stages = return $ Right out
                         | otherwise = do
            putStrLn $ "Task: " ++ show x
            core <- either return loadCore inp
            (err,core2) <- f core
            when (Text `elem` options) $ writeFile (out <.> "txt") (show core2)
            when (Html `elem` options) $ writeFile (out <.> "html") (coreHtml core2)
            saveCore out core2
            when (not $ null err) $ error err
            return $ Left core2



findFileHs :: String -> IO FilePath
findFileHs file = do
    b <- doesFileExist file
    if b then return file else do
        let files = ["../examples" </> s </> file <.> "hs" | s <- ["Example","Nofib"]]
        bs <- mapM doesFileExist files
        case [a | (a,b) <- zip files bs, b] of
            (x:_) -> return x
            _ -> error $ "File not found, " ++ file

