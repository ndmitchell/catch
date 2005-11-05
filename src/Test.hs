
module Test(test) where

import General
import Hite
import Directory
import TextUtil


data Mode = Interactive | Batch String String


test :: [String] -> IO ()
test xs = do testItem Interactive xs
             putStrLn "Completed"


testItem :: Mode -> [String] -> IO ()
testItem m ("-hite":xs) = testHite m xs
testItem m xs = mapM_ testFolder xs


testFolder :: FilePath -> IO ()
testFolder x = do isDir <- doesDirectoryExist x
                  isFile <- doesFileExist x
                  if isDir then
                      do xs <- getDirectoryContents x
                         mapM_ (testFolder . (++) (x ++ "/")) $ filter isReal xs
                   else if isFile then
                      testFile x
                   else
                      putStrLn $ "ERROR: Not a file or folder, " ++ x
    where
        isReal "." = False
        isReal ".." = False
        isReal "CVS" = False
        isReal _ = True


testFile :: FilePath -> IO ()
testFile file = do x <- readFile file
                   let xs = lines x
                       params = splitList " " (xs !! 0)
                       brk = xs !! 1
                       (input, _:output) = break (== brk) (drop 2 xs)
                   putStrLn $ "Check: " ++ file
                   testItem (Batch (unlines input) (unlines output)) params
                   


testHite :: Mode -> [String] -> IO ()
testHite m ("-forward":files) = checkHiteId m forward files
testHite m ("-reachable":xs:files) = checkHiteId m (reachable xs) files
testHite m ("-check":files) = checkHiteRun check files
testHite m ("-inline":files) = checkHiteId m inline files
testHite m ("-firstify":files) = checkHiteId m firstify files
testHite m ("-read":files) = checkHiteId m id files
testHite m [] = putStrLn "ERROR: You must specify what test to run after -hite"
testHite m (x:xs) = putStrLn $ "ERROR: Unknown test, " ++ x


exec :: Mode -> [FilePath] -> (String -> String) -> (String -> String -> Bool) -> IO ()
exec Interactive []    f eq = putStrLn "No files to check against"

exec Interactive files f eq = mapM_ g files
    where g src = do x <- readFileMaybe src
                     case x of
                        Just x -> do putStrLn $ "Check: " ++ src
                                     putStrLn (f x)
                        Nothing -> putStrLn $ "ERROR: File not found, " ++ src
                   
exec (Batch input output) _ f eq = if eq (f input) output then return () else
        putStrLn $ unlines 
            [
                "ERROR: Output does not match",
                "Expected:",
                g output,
                "Found:",
                g (f input)
            ]
    where
        g = unlines . map ("  |  " ++) . lines
        

checkHiteId :: Mode -> (Hite -> Hite) -> [FilePath] -> IO ()
checkHiteId m f xs = exec m xs (show . f . read) (\a b -> f (read a) == read b)

checkHiteRun :: Show a => (Hite -> a) -> [FilePath] -> IO ()
checkHiteRun f xs = exec Interactive xs (show . f . read) eqHiteStr
    where
        eqHiteStr a b = read a `eqHite` read b
        
        eqHite :: Hite -> Hite -> Bool
        eqHite = (==)



