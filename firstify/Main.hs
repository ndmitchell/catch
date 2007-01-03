
module Main where

import System.FilePath
import Yhc.Core
import System.Environment
import System.Directory
import Data.Maybe
import Data.List


main = do
        x <- getArgs
        mapM_ f x
    where
        f fil = do
            file <- findFile fil
            core <- loadCore file
            let file2 = replaceExtension file "first.yca"
                (fo,core2) = firstify $ mapUnderCore remCorePos $ lambdas $ zeroApp core
            saveCore file2 core2
            print core2
            putStrLn $ "-- " ++ (if fo then "FIRST" else "HIGHER") ++ " order"



findFile :: String -> IO FilePath
findFile file = do
    bs <- mapM doesFileExist files
    case [a | (a,b) <- zip files bs, b] of
        (x:_) -> return x
        _ -> error $ "File not found, " ++ file
    where files = file : ["../examples" </> s </> "ycr" </> file <.> "over.yca" | s <- ["Example","Nofib"]]



lambdas :: Core -> Core
lambdas core = mapUnderCore f $ removeRecursiveLet core
    where
        f orig@(CoreApp (CoreFun name) args) | extra > 0 =
                CoreLam new (CoreApp (CoreFun name) (args ++ map CoreVar new))
            where
                extra = arity core name - length args
                new = take extra $ ['v':show i | i <- [1..]] \\ collectAllVars orig
        f x = x


zeroApp :: Core -> Core
zeroApp = mapUnderCore f
    where
        f (CoreFun x) = CoreApp (CoreFun x) []
        f (CoreApp (CoreApp x ys) zs) = CoreApp x (ys++zs)
        f x = x



firstify :: Core -> (Bool, Core)
firstify x = f 10 emptySpec x
    where
        f n s x | not $ hasHO x = (True, x)
                | n == 0 = (False, x)
                | otherwise = uncurry (f (n-1)) (process s x)

        process spec core = (spec2, core3)
            where
                core3 = coreReachable ["main"] $ coreSimplify core2
                (spec2,core2) = specHO spec $ coreSimplify $ inlineHO core


arity :: Core -> String -> Int
arity core name = length $ coreFuncArgs $ coreFunc core name


hasHO :: Core -> Bool
hasHO core = any isCoreLam (allCore core)


isHO :: Core -> CoreExpr -> Bool
isHO core (CoreLet _ x) = isHO core x
isHO core (CoreLam _ _) = True
isHO core (CoreCase x ys) = any (isHO core . snd) ys
isHO core (CoreApp (CoreCon _) args) = any (isHO core) args
isHO core _ = False


inlineHO :: Core -> Core
inlineHO core = mapUnderCore f $ zeroApp core
    where
        inline = [(coreFuncName func, func) | func <- coreFuncs core, isHO core $ coreFuncBody func]
        
        f (CoreApp (CoreFun name) args) | isJust func = coreLam extra rest
            where
                func = lookup name inline
                (extra,rest) = coreInlineFuncLambda (fromJust func) args

        f x = x



type Special = [Spec]
data Spec = Spec String [CoreExpr] String
            deriving Show

instance Eq Spec where
    (Spec a b _) == (Spec c d _) = a == c && b == d


emptySpec = []

specHO :: Special -> Core -> (Special, Core)
specHO spec core = (spec2, core3)
    where
        core3 = useSpecial core2 spec2
        core2 = core{coreFuncs = func2 ++ coreFuncs core}
        func2 = genSpecial core new
        spec2 = new ++ spec
        new = nameSpecial core $ askSpecial core \\ spec





askSpecial :: Core -> [Spec]
askSpecial core = nub $ mapMaybe (wantSpecial core) $ allCore core


wantSpecial :: Core -> CoreExpr -> Maybe Spec
wantSpecial core (CoreApp (CoreFun x) xs) | any (isHO core) xs = Just $ f x xs
    where
        f x xs = Spec x xs2 ""
            where CoreApp _ xs2 = g (CoreApp (CoreFun x) (zipWith h [0..] xs))

        h n x = if isHO core x then replaceFreeVars rep x else CoreVar ('_':show n)
            where rep = zip (collectFreeVars x) [CoreVar $ 'v':show n ++ "_" ++ show i | i <- [1..]]
        
        g = repFree ['v':show n | n <- [1..]] . repFree ['$':show n | n <- [1..]]
        
        repFree new x = replaceFreeVars (zip frees (map CoreVar freenew)) $ uniqueFreeVarsWith othernew x
            where
                (freenew,othernew) = splitAt (length frees) new
                frees = collectFreeVars x
wantSpecial _ _ = Nothing


nameSpecial :: Core -> [Spec] -> [Spec]
nameSpecial core xs = f (map coreFuncName $ coreFuncs core) xs
    where
        f seen [] = []
        f seen (Spec func args _ : rest) = Spec func args newname : f (newname:seen) rest
            where newname = head $ [name | i <- [1..], let name = func ++ "_" ++ show i, name `notElem` seen]


genSpecial :: Core -> [Spec] -> [CoreFunc]
genSpecial core specs = map f specs
    where
        f (Spec func args name) = CoreFunc name (collectFreeVars (CoreApp (CoreFun func) args)) body2
            where
                CoreFunc _ params body = coreFunc core func
                body2 = replaceFreeVars (zip params args) body


useSpecial :: Core -> Special -> Core
useSpecial core spec = mapUnderCore f core
    where
        f o@(CoreApp (CoreFun x) xs) | isJust ms && fromJust ms `elem` spec =
                CoreApp (CoreFun name) (concatMap g xs)
            where
                Spec _ args name = head $ filter (==fromJust ms) spec
                ms = wantSpecial core o
                
                g x | isHO core x = map CoreVar $ collectFreeVars x
                    | otherwise = [x]
                
        f x = x

