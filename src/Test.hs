
module Test(test) where

import Hite


data Mode = Interactive | Batch String String


test :: [String] -> IO ()
test = testItem Interactive


testItem :: Mode -> [String] -> IO ()
testItem m ("-hite":xs) = testHite m xs
testItem m xs = mapM_ testFolder xs


testFolder :: FilePath -> IO ()
testFolder = error "todo"


testFile :: FilePath -> IO ()
testFile = error "todo"


testHite :: Mode -> [String] -> IO ()
testHite m ("-forward":files) = checkHiteId m forward files
testHite m ("-reachable":xs:files) = checkHiteId m (reachable xs) files
testHite m ("-check":files) = checkHiteRun check files
testHite m ("-inline":files) = checkHiteId m inline files
testHite m ("-firstify":files) = error "todo"
testHite m ("-read":files) = checkHiteId m id files



exec :: Mode -> [FilePath] -> (String -> String) -> (String -> String -> Bool) -> IO ()
exec Interactive files f eq = mapM_ g files
    where g src = do x <- readFile src
                     putStrLn (f x)
                   
exec (Batch input output) _ f eq = if eq input output then return () else putStrLn "error!"


checkHiteId :: Mode -> (Hite -> Hite) -> [FilePath] -> IO ()
checkHiteId m f xs = exec m xs (show . f . read) (\a b -> f (read a) == read b)

checkHiteRun :: Show a => (Hite -> a) -> [FilePath] -> IO ()
checkHiteRun f xs = exec Interactive xs (show . f . read) (==)



