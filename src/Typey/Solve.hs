
module Typey.Solve(typeySolve) where

import Hite
import Typey.Type
import Typey.Pretty
import Data.List
import Data.Maybe
import Data.Predicate
import General.General
import IO
import Debug.Trace


type State = (Hite, DataM SmallT, FuncM)


data Item = Item FuncName [Subtype] Subtype Pending

data Pending = Never
             | Later
             | Now Subtype
             deriving Show


instance Show Item where
    showList xs = showString $ "\n" ++ unlines (map show xs)
    show (Item name args free pending) =
        name ++ " :: " ++
        (concat $ intersperse " -> " $ map show $ args ++ [free]) ++
        " = " ++ show pending

        
-- return the subtypes of main that do not have bottom in
typeySolve :: String -> Handle -> Hite -> DataM SmallT -> FuncM -> IO Bool
typeySolve file hndl hite datam funcm =
    do
        outBoth "-- FUNCTION TYPES"
        out $ showLines funcm
        outBoth "-- ORIGINAL TYPES"
        out $ show orig
        outBoth "-- EXPANDED TYPES"
        out $ show expand
        outBoth "-- EQUAL VARIABLE SETS"
        out $ showLines ans
        outBoth "-- GENERATING FILE"
        typeyPretty ("Logs/" ++ file) (("!root",resultArg,resultRes) : instantiateAll ans expand)
        outBoth "-- INSTANTIATE ANSWER"
        outBoth $ show (resultArg ++ [resultRes])
        outBoth "-- FINAL ANSWER"
        outBoth $ if resultBool then "Safe :)" else "Unsafe :("
        return resultBool
    where
        out = hPutStrLn hndl
        outBoth x = putStrLn x >> out x
    
        resultBool = not $ any hasBottom [instantiate ans x | Item "main" _ x _ <- expand]
        (resultArg, resultRes) = answer expand ans
        ans = fixpVariables expand n2
        (n2,expand) = expandRhs state orig n
        (n,orig) = addItems state [] pairings 0
        pairings = [(fname,args) | name <- funcs hite, let fname = funcName name, fname /= "error",
                                   args <- getSubtypesFunc datam $ lookupNote "Typey.Solve.typeySolve" fname funcm]
        state = (hite,datam,funcm)


showLines :: Show x => [x] -> String
showLines = unlines . map show

instantiateAll :: Results -> [Item] -> [(FuncName, [Subtype], Subtype)]
instantiateAll res xs = [(x,y,instantiate res z) | Item x y z _ <- xs]


instantiate :: Results -> Subtype -> Subtype
instantiate res x = replace x
    where
        replace :: Subtype -> Subtype
        replace (Subtype a b) = Subtype (f a) (f b)
            where f (a :@ b) = rep a :@ map replace b
        replace (Atom x) = Atom (rep x)
        
        rep :: [Subvalue] -> [Subvalue]
        rep xs = concatMap repOne xs
        
        repOne :: Subvalue -> [Subvalue]
        repOne (SVar x) = filter (not . isVar) $ concat [y | y <- res, SVar x `elem` y]
            where isVar (SVar _) = True; isVar _ = False
        repOne x = [x]


answer :: [Item] -> Results -> ([Subtype], Subtype)
answer xs rep = foldr1 f (if null success then mains else success)
    where
        f (as,a) (bs,b) = (zipWithEq unionPair as bs, unionPair a b)
    
        success = filter (not . hasBottom . snd) mains
        mains = [(args, instantiate rep res)  | Item "main" args res _ <- xs]
        


addItems :: State -> [Item] -> [(FuncName, [Subtype])] -> Int -> (Int, [Item])
addItems (hite,datam,funcm) items add norig = (n, items ++ i2)
    where
        (n,i2) = mapId f add norig
    
        f (name, arg) n = (n2, Item name arg r2 Later)
            where
                (FuncT _ _ res) = lookupJust name funcm
                (n2,r2) = getSubtypeFree datam res n


-- all rhs's must stop being Later
expandRhs :: State -> [Item] -> Int -> (Int, [Item])
expandRhs state@(hite,datam,funcm) xs n = (n2, map head xs2 ++ concatMap tail xs2)
    where
        (n2,xs2) = mapId f xs n
        
        f (Item name args free Later) n = (n2, Item name args free (Now res2) : concatMap snd es)
            where
                res2 = if any (hasBottom . fst) es then resBot else res
                resBot = allBottom free
                res = unionListNote "expandRhs.f" (map fst es)
                ren = zip fargs args
                (n2, es) = mapId (getTypeCall ren) exprs n
                exprs = [expr | MCaseAlt p expr <- opts, doesMatch ren p]
                (Func _ fargs (MCase opts) _) = getFunc hite name

        getTypeCall :: [(String, Subtype)] -> Expr -> Int -> (Int, (Subtype, [Item]))
        getTypeCall ren x n
            | callDepth x <= 1 = (n, (getType ren x, []))
            | otherwise = (n2, (newres, [newitem]))
            where
                Call (CallFunc name) args = x
                sargs = map (getType ren) args
                (FuncT _ _ res) = lookupJust name funcm
                (n2, newres) = getSubtypeFree datam res n
                newitem = Item name sargs newres Never



        doesMatch :: [(String, Subtype)] -> Pred MCaseOpt -> Bool
        doesMatch ren x = mapPredBool f x
            where
                f (MCaseOpt x c) = SCtor c `elem` a
                    where (Subtype (a :@ _) _) = getExpr ren x


        getExpr :: [(String, Subtype)] -> Expr -> Subtype
        getExpr ren (Var x) = lookupJust x ren
        getExpr ren (Sel x y) = res
            where
                s@(Subtype (_ :@ inside) _) = getExpr ren x
                ctor = getCtorFromArg hite y
                cname = ctorName ctor
                dname = dataName $ getDataFromCtor hite cname
                dtype@(DataT free ctors) = lookupJust dname datam
                cargs = head [xs | CtorT n xs <- ctors, n == cname]
                argPos = fromJust $ elemIndex y (ctorArgs ctor)
                res = case cargs !! argPos of
                          Self -> liftSubtype s
                          FreeS i -> inside !! i


        getType :: [(String, Subtype)] -> Expr -> Subtype
        getType ren (Make name xs) = foldr f (Subtype ([SCtor name]:@blanks) ([]:@blanks)) (zip cargs xs2)
            where
                blanks = replicate free Empty
                dname = dataName $ getDataFromCtor hite name
                dtype@(DataT free ctors) = lookupJust dname datam
                cargs = head [xs | CtorT n xs <- ctors, n == name]
                xs2 = map (getType ren) xs
                
                f (Self, x) y = collapseSubtype x `unionPair` y
                f (FreeS i, x) (Subtype (a:@b) d) = Subtype (a :@ (b !!! (i, (b !! i) `unionPair` x))) d

        getType ren x | isVar x || isSel x = getExpr ren x
        
        getType ren (Call (CallFunc name) args) = getCall name typs
            where typs = map (getType ren) args


        getCall :: FuncName -> [Subtype] -> Subtype
        getCall "error" _ = Atom [Bot]
        getCall name args = unionListNote ("getCall, " ++ name ++ " " ++ show args)
            [x | Item n a x Later <- xs, n == name, and $ zipWithEq isSubset a args]


unionListNote msg [] = error $ "unionListNote, " ++ msg
unionListNote msg xs = unionList xs


type Results = [[Subvalue]]

fixpVariables :: [Item] -> Int -> Results
fixpVariables xs n = fixp baseResults
    where
        baseResults = makeResults $ concatMap eqItem xs
        
        fixp :: Results -> Results
        fixp res = if null newres then res
                   else fixp $ makeResults $ newres ++ res
            where
                newres = filter isNew $ addConstraints res
                isNew r = not $ any (r `isSubset`) res
                isSubset a b = null (a \\ b)
        
        
        makeResults = simpSets . joinAllSets
    
        eqItem :: Item -> Results
        eqItem (Item _ _ a (Now b)) = eqSubtype a b
        eqItem _ = []
        
        eqSubtype :: Subtype -> Subtype -> Results
        eqSubtype (Atom a1) (Atom a2) = [a1++a2]
        eqSubtype (Subtype a1 b1) (Subtype a2 b2) = f a1 a2 ++ f b1 b2
            where
                f (a1:@b1) (a2:@b2) = eqSubtype (Atom a1) (Atom a2) ++ concat (zipWithEq eqSubtype b1 b2)
        eqSubtype _ Empty = []
        eqSubtype Empty _ = []

        simpSets :: Results -> Results
        simpSets x = concatMap f x
            where
                f [] = []
                f x = [nub x]
    
    
        joinAllSets :: Results -> Results
        joinAllSets x = f 0 x
            where
                f i x | i == n = x
                      | otherwise = f (i+1) (joinSets x i)
        
        joinSets :: Results -> Int -> Results
        joinSets rews n = concat a : b
            where (a,b) = partition (SVar n `elem`) rews

        
        -- what constraints have to be added because of the Later statements
        addConstraints :: Results -> Results
        addConstraints res = concatMap f xs
            where
                f (Item name args free Never) = free `eqSubtype` unionList
                        [instantiate res fre | Item nam arg fre (Now _) <- xs, nam == name,
                            and $ zipWithEqNote ("addConstraints, " ++ name) isSubset (map (instantiate res) arg) args2]
                    where args2 = map (instantiate res) args
                f _ = []



mapId :: (a -> Int -> (Int, b)) -> [a] -> Int -> (Int, [b])
mapId f [] n = (n,[])
mapId f (x:xs) n = (n3, x2:x3)
    where
        (n2,x2) = f x n
        (n3,x3) = mapId f xs n2



getSubtypeFree :: DataM SmallT -> LargeT -> Int -> (Int, Subtype)
getSubtypeFree datam (FreeL i) n = (n+1, Atom [SVar n])
getSubtypeFree datam (CtorL name xs) n
        | recursive = let (n1,a) = f n
                          (n2,b) = f n1
                      in (n2, Subtype a b)
        | otherwise = let (n1,a) = f n
                      in (n1, Subtype a ([]:@[]))
    where
        f n = (a,[SVar n] :@ b)
            where (a,b) = mapId (getSubtypeFree datam) xs (n+1)
    
        (DataT free ctors) = lookupJust name datam
        recursive = any isRecursive ctors
