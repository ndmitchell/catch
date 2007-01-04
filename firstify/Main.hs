
module Main where

import System.FilePath
import Yhc.Core
import System.Environment
import System.Directory
import Data.Maybe
import Data.List
import Debug.Trace


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
            writeFile (file2 <.> "html") (coreHtml core2)
            
            src <- readFile "Prefix.txt"
            writeFile (file2 <.> "hs") (src ++ coreHaskell (prepare core2))
            
            print core2
            putStrLn $ "-- " ++ (if fo then "FIRST" else "HIGHER") ++ " order"


prepare :: Core -> Core
prepare = prepareData . mapUnderCore f
    where
        f (CorePrim xs) = CorePrim $ "prim_" ++ map (\x -> if x == '.' then '_' else x) xs
        f (CoreChr x) = CoreApp (CorePrim "prim_CHR") [CoreChr x]
        f (CoreStr x) = CoreApp (CorePrim "prim_STRING") [CoreStr x]
        f x = x


prepareData core = core{coreDatas = extraData ++ filter ((`notElem` used) . coreDataName) (coreDatas core)}
    where used = map coreDataName extraData


extraData = [CoreData "()" [] [CoreCtor "()" []]
            ,CoreData "Primitive.IO" ["a"] [CoreCtor "Primitive.IO" [("a",Nothing)]]
            ]


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
                | n == 0 = trace "Giving up!" (False, x)
                | otherwise = uncurry (f (n-1)) (process s x)

        process spec core = (spec2, core3)
            where
                (spec2,core2) = fromMaybe (spec,core) $ specHOs spec core
                core3 = fromMaybe core2 $ inlineHO core2


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


isDataHO :: Core -> CoreExpr -> Bool
isDataHO core (CoreApp (CoreCon _) args) = any (isHO core) args
isDataHO core (CoreLet _ x) = isDataHO core x
isDataHO core (CoreCase x ys) = any (isDataHO core . snd) ys
isDataHO _ _ = False


inlineHOs :: Core -> Maybe Core
inlineHOs x | isJust x2 = Just $ f $ fromJust x2
            | otherwise = Nothing
    where
        x2 = inlineHO x
    
        f x = case inlineHO x of
                  Nothing -> x
                  Just y -> f y


inlineHO :: Core -> Maybe Core
inlineHO core | null inline = Nothing
              | otherwise = Just $ coreReachable ["main"] $ coreSimplify $ mapUnderCore f $ zeroApp core
    where
        inline = [(coreFuncName func, func) | func <- coreFuncs core, isHO core $ coreFuncBody func] --, canInline func]
        
        canInline func = null [() | CoreFun name <- allCore (coreFuncBody func), name == coreFuncName func]
        
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


specHOs :: Special -> Core -> Maybe (Special, Core)
specHOs spec core = case specHO spec core of
                        Nothing -> Nothing
                        Just (s2,c2) -> f s2 c2
    where
        f s c = case specHO s c of
                    Nothing -> Just (s,c)
                    Just (s,c) -> f s c
        

specHO :: Special -> Core -> Maybe (Special, Core)
specHO spec core = if changed || not (null new) then Just (spec2, core4) else Nothing
    where
        core4 = coreReachable ["main"] $ coreSimplify $ core3
        (changed,core3) = useSpecial core2 spec2
        core2 = core{coreFuncs = func2 ++ coreFuncs core}
        func2 = genSpecial core new
        spec2 = new ++ spec
        new = nameSpecial core $ askSpecial core \\ spec





askSpecial :: Core -> [Spec]
askSpecial core = nub $ mapMaybe (wantSpecial core) $ allCore core


wantSpecial :: Core -> CoreExpr -> Maybe Spec
wantSpecial core (CoreApp (CoreFun x) xs) | nxs >= ar && (nxs > ar || any (isHO core) xs) = Just $ f x xs
    where
        nxs = length xs
        ar = arity core x
    
        f x xs = Spec x xs2 ""
            where CoreApp _ xs2 = g (CoreApp (CoreFun x) (zipWith h [0..] xs))

        h n x = if isHO core x then replaceFreeVars rep x else CoreVar ('_':show n)
            where rep = zip (collectFreeVars x) [CoreVar $ 'v':show n ++ "_" ++ show i | i <- [1..]]
        
        g = repFree ['v':show n | n <- [1..]] . repFree ['$':show n | n <- [1..]]
        
        repFree new x = replaceFreeVars (zip frees (map CoreVar freenew)) $ uniqueBoundVarsWith othernew x
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
        f (Spec func args name) = CoreFunc name args2 body3
            where
                args2 = collectFreeVars (CoreApp (CoreFun func) args)
                CoreFunc _ params body = coreFunc core func
                body3 = coreApp (replaceFreeVars (zip params args) body2) (drop (length params) args)
                body2 = uniqueBoundVarsWithout (collectAllVars (CoreApp (CoreFun func) args)) body


useSpecial :: Core -> Special -> (Bool,Core)
useSpecial core spec = (core2 /= core, core2)
    where
        core2 = mapUnderCore f core

        f o@(CoreApp (CoreFun x) xs) | isJust ms && fromJust ms `elem` spec =
                CoreApp (CoreFun name) (concatMap g used ++ extra)
            where
                (used,extra) = splitAt (arity core x) xs
                Spec _ args name = head $ filter (==fromJust ms) spec
                ms = wantSpecial core o
                
                g x | isHO core x = map CoreVar $ collectFreeVars x
                    | otherwise = [x]
                
        f x = x
