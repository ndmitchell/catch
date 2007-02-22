
module Main where

import System.Environment
import System.Directory
import System.FilePath
import System.Cmd
import Data.Maybe

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
    let core2 = coreReachable ["main"] $ coreOverlay (abstract src) over
    saveCore (replaceExtension file "over.yca") core2
    writeFile (replaceExtension file "over.yca.html") (coreHtml core2)



abstract = litAbstract . primAbstract . caseAbstract


---------------------------------------------------------------------
-- Remove cases on constants
caseAbstract :: Core -> Core
caseAbstract core = mapUnderCore f core
    where
        f (CoreCase on alts) | any (isCoreChr . fst) alts
            = CoreCase on [(CoreApp (CoreCon "Primitive.Char") [], anys (map snd alts))]

        f x@(CoreCase on alts) | any (isCoreConst . fst) alts
            = CoreCase on (cas "Neg" neg ++ cas "Zero" zero ++ cas "One" one ++ cas "Pos" pos ++ def)
            where
                zero = let x = pick "Zero" in if null x then other else x
                one  = let x = pick "One"  in if null x then other else x
                pos = other ++ pick "Pos"
                neg = other ++ pick "Neg"
                other = pick ""
            
                pick x = map snd $ filter ((==) x . g . fst) alts
                g x = if isCoreConst x then constAbstract x else ""
                
                def = [(CoreVar "_", anys other) | any (== other) [neg, zero, one, pos]]
                cas x rs = [(CoreApp (CoreCon ("Primitive." ++ x)) [], anys rs) | rs /= other]

        f x = x
        
        
        anys [x] = x
        anys xs = CoreApp (CoreFun $ "Primitive.any" ++ show (length xs)) xs
   

---------------------------------------------------------------------
-- Remove cases on constants


numPrims = [(PrimAdd, "numAdd"), (PrimSub, "numSub"), (PrimMul, "numMul")
           ,(PrimDiv, "numDiv"), (PrimQuot, "numQuot"), (PrimRem, "numRem")
           ,(PrimEq, "numEq"), (PrimNe, "numNe"), (PrimLt, "numLt"), (PrimGt, "numGt")
           ]


primAbstract :: Core -> Core
primAbstract = mapUnderCore f
    where
        f (CoreFun  x) = g CoreFun  x
        f (CorePrim x) = g CorePrim x
        f x = x
        
        g rebuild x = fromMaybe (rebuild x) $ do
                          prim <- corePrimMaybe x
                          func <- lookup (primOp prim) numPrims
                          return $ CoreFun $ "Primitive." ++ func


litAbstract :: Core -> Core
litAbstract = mapUnderCore f
    where
        f x | isPrimConst x = CoreApp (CoreCon ("Primitive." ++ constAbstract x)) []
        f x = x


isPrimConst x = isCoreConst x && not (isCoreStr x)


constAbstract :: CoreExpr -> String
constAbstract x = f x
    where
        f (CoreChr     x) = "Char"
        f (CoreInt     x) = number x
        f (CoreInteger x) = number x
        f (CoreDouble  x) = number x
        f (CoreFloat   x) = number x

        number x = if x < 0 then "Neg"
                   else if x == 0 then "Zero"
                   else if x == 1 then "One"
                   else "Pos"
