
module Hill.File(cmdsFile) where

import Hill.Type
import Hill.Cache
import qualified Data.Set as Set
import Yhc.Core
import Convert.CoreHill
import Front.CmdLine
import Control.Monad
import System.FilePath
import Data.List
import System


cmdsFile =
    [Action "core-make" coreMake
    ,Action "core-load" coreLoad
    ,Action "core-hill" coreHill
    ,Action "overlay-make" overlayMake
    ,Action "overlay-apply" overlayApply
     -- ,Action "core-save" coreSave
    ,Action "hill-load" hillLoad
    ,Action "hill-save" hillSave
    ]



coreMake :: CmdLineState -> String -> Value -> IO Value
coreMake state _ (ValueFile x) = system ("yhc -hide -linkcore " ++ x) >> return (ValueFile x)

overlayMake :: CmdLineState -> String -> Value -> IO Value
overlayMake state _ val = system "yhc -hide -core Library/Primitive.hs" >> return val

coreLoad :: CmdLineState -> String -> Value -> IO Value
coreLoad state _ (ValueFile x) = liftM ValueCore $ loadCore x2
    where x2 = dropFileName x </> "ycr" </> getBaseName x <.> "yca"

coreHill :: CmdLineState -> String -> Value -> IO Value
coreHill state _ (ValueCore x) = return $ ValueHill $ convHill x


overlayApply :: CmdLineState -> String -> Value -> IO Value
overlayApply state _ (ValueCore x) = do
    prim <- liftM decodePrimitive $ loadCore "Library/ycr/Primitive.ycr"
    let ignore = Set.fromList $ map coreFuncName $ coreFuncs prim
    let x2 = x{coreDatas = coreDatas prim ++ coreDatas x
              ,coreFuncs = coreFuncs prim ++ filter ((`Set.notMember` ignore) . coreFuncName) (coreFuncs x)}
    return $ ValueCore x2



decodePrimitive :: Core -> Core
decodePrimitive core = core{coreFuncs = mapOverCore f $ map g $ coreFuncs core}
    where
        g func = func{coreFuncName = decodeGlobal $ coreFuncName func}
        
        f (CoreFun x) = CoreFun $ decodeGlobal x
        f x = x
    
        


decodeGlobal :: String -> String
decodeGlobal x | "Primitive.global_" `isPrefixOf` x = f (drop 17 x)
               | otherwise = x
    where
        f ('\'':'\'':xs) = '\'' : f xs
        f ('\'':'_':xs) = '.' : f xs
        f ('\'':'g':'t':xs) = '>' : f xs
        f ('\'':'e':'q':xs) = '=' : f xs
        f (x:xs) = x : f xs
        f [] = []





hillLoad :: CmdLineState -> String -> Value -> IO Value
hillLoad state _ _ = liftM ValueHill $ readCacheHill $ cmdLineOutput state "hill"

hillSave :: CmdLineState -> String -> Value -> IO Value
hillSave state _ value = writeCacheHill (valueHill value) (cmdLineOutput state "hill") >> return value
