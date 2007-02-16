
module Main where

import System.Environment
import System.Directory
import System.FilePath
import System.Cmd

import Yhc.Core


main = getArgs >>= mapM_ prepFile



prepFile :: String -> IO ()
prepFile fil = do
    fileHs <- findFileHs fil
    fileYha <- findFileYha fileHs
    compile fileHs
    overlay fileYha



findFileHs :: String -> IO FilePath
findFileHs file = do
    b <- doesFileExist file
    if b then return file else do
        let files = ["../examples" </> s </> file <.> "hs" | s <- ["Example","Nofib"]]
        bs <- mapM doesFileExist files
        case [a | (a,b) <- zip files bs, b] of
            (x:_) -> return x
            _ -> error $ "File not found, " ++ file


findFileYha :: String -> IO FilePath
findFileYha x = return $ dropFileName x </> "ycr" </> replaceExtension (takeFileName x) "yca"



compile :: String -> IO ()
compile file = do
    system $ "yhc -hide -linkcore " ++ file
    return ()


overlay :: String -> IO ()
overlay file = do
    system "yhc -hide -core ../examples/Library/Primitive.hs"
    src <- loadCore file
    over <- loadCore "../examples/Library/ycr/Primitive.ycr"
    let core2 = coreReachable ["main"] $ coreOverlay (numAbstract src) over
    saveCore (replaceExtension file "over.yca") core2



numPrims = [("ADD_W","numAdd"),("SUB_W","numSub")
           ,("LT_W","numLT"),("GT_W","numGT")
           ,("QUOT","numQuot"),("REM","numRem"),("SLASH_D","numDiv")
           ,("YHC.Primitive.primIntegerEq","numEq")
           ,("YHC.Primitive.primIntegerQuot","numQuot")
           ,("YHC.Primitive.primDoubleFromInteger","numId")
           ,("YHC.Primitive.primIntegerFromInt","numId")
           ]


numAbstract :: Core -> Core
numAbstract core = mapUnderCore f core
    where
        f (CoreChr x) = CoreApp (CoreCon "Primitive.Char") []
        f (CoreInt x) = number x
        f (CoreInteger x) = number x
        f (CoreDouble x) = number x
        f (CoreFloat x) = number x
        
        f (CoreFun x) = case lookup x numPrims of
                            Nothing -> CoreFun x
                            Just y -> CoreFun ("Primitive." ++ y)
        
        f (CorePrim x) = case lookup x numPrims of
                            Nothing -> CorePrim x
                            Just y -> CoreFun ("Primitive." ++ y)
        f x = x


        number x = CoreApp (CoreCon ("Primitive." ++ s)) []
            where s = if x == 0 then "Zero" else if x < 0 then "Neg" else "Pos"
