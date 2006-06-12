
module Typey.Solve(typeySolve) where

import Hite
import Typey.Type
import Typey.Pretty
import Data.List
import Data.Maybe
import Data.Predicate
import General.General


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
typeySolve :: Hite -> DataM SmallT -> FuncM -> IO Subtype
typeySolve hite datam funcm = do putStrLn "-- EXPANDED TYPES"
                                 print expand
                                 putStrLn "-- EQUAL VARIABLE SETS"
                                 print ans
                                 putStrLn "-- GENERATING FILE"
                                 typeyPretty "Logs/New" $ instantiateAll ans expand
                                 putStrLn "-- INSTANTIATE"
                                 print res
                                 return $ error "todo"
    where
        res = answer expand ans
        ans = fixpVariables expand n2
        (n2,expand) = expandRhs state orig n
        (n,orig) = addItems state [] pairings 0
        pairings = [(fname,args) | name <- funcs hite, let fname = funcName name, fname /= "error",
                                   args <- getSubtypesFunc datam $ fromJust $ lookup fname funcm]
        state = (hite,datam,funcm)


instantiateAll :: [[Result]] -> [Item] -> [(FuncName, [Subtype], Subtype)]
instantiateAll res xs = [(x,y,instantiate res z) | Item x y z _ <- xs]


instantiate :: [[Result]] -> Subtype -> Subtype
instantiate res x = replace x
    where
        replace :: Subtype -> Subtype
        replace (Subtype a b c d) = Subtype (concatMap rep a) (concatMap rep b) (map replace c) (map replace d)
        replace (SVar x) = case getVar (head x) of
                              RTop -> Top
                              RBot -> Bot
                              _ -> SVar x
        replace x = x
        
        
        rep :: UCtor -> [UCtor]
        rep (UVar x) = case getVar x of
                            RCtor x -> map UCtor x
                            _ -> [UVar x]
        rep x = [x]
        
        getVar :: Int -> Result
        getVar n = if RBot `elem` xs then RBot
                   else if null xs then RTop
                   else RCtor (concat [x | RCtor x <- xs])
            where
                xs = filter f $ head $ filter (RVar n `elem`) res
                
                f (RVar _) = True
                f RTop = True
                f _ = False
                



answer :: [Item] -> [[Result]] -> Subtype
answer xs res = unionSubtype [y | Item "main" _ x _ <- xs, let y = instantiate res x, noBottom y]
    where
        noBottom :: Subtype -> Bool
        noBottom (Subtype a b c d) = all noBottom (c++d)
        noBottom Bot = False
        noBottom _ = True
        


addItems :: State -> [Item] -> [(FuncName, [Subtype])] -> Int -> (Int, [Item])
addItems (hite,datam,funcm) items add norig = (n, items ++ i2)
    where
        (n,i2) = mapId f add norig
    
        f (name, arg) n = (n2, Item name arg r2 Later)
            where
                (FuncT _ _ res) = fromJust $ lookup name funcm
                (n2,r2) = getSubtypeFree datam res n


-- all rhs's must stop being Later
expandRhs :: State -> [Item] -> Int -> (Int, [Item])
expandRhs state@(hite,datam,funcm) xs n = (n2, concat xs2)
    where
        (n2,xs2) = mapId f xs n
        
        f (Item name args free Later) n = (n, [Item name args free (Now res)])
            where
                res = unionSubtype $ map (getType ren) exprs
                ren = zip fargs args
                exprs = [expr | MCaseAlt p expr <- opts, doesMatch ren p]
                (Func _ fargs (MCase opts) _) = getFunc hite name


        doesMatch :: [(String, Subtype)] -> Pred MCaseOpt -> Bool
        doesMatch ren x = demandBool $ mapPredLit f x
            where
                f (MCaseOpt x c) = predBool $ c `elem` [b | UCtor b <- a]
                    where (Subtype a _ _ _) = getExpr ren x


        getExpr :: [(String, Subtype)] -> Expr -> Subtype
        getExpr ren (Var x) = fromJust $ lookup x ren
        getExpr ren (Sel x y) = res
            where
                s@(Subtype a b c d) = getExpr ren x
                ctor = getCtorFromArg hite y
                cname = ctorName ctor
                dname = dataName $ getDataFromCtor hite cname
                dtype@(DataT free ctors) = fromJust $ lookup dname datam
                cargs = head [xs | CtorT n xs <- ctors, n == cname]
                argPos = fromJust $ elemIndex y (ctorArgs ctor)
                res = case cargs !! argPos of
                          Self -> liftSubtype s
                          FreeS i -> c !! i


        getType :: [(String, Subtype)] -> Expr -> Subtype
        getType ren (Make name xs) = foldr f (Subtype [UCtor name] [] blanks blanks) (zip cargs xs2)
            where
                blanks = replicate free Empty
                dname = dataName $ getDataFromCtor hite name
                dtype@(DataT free ctors) = fromJust $ lookup dname datam
                cargs = head [xs | CtorT n xs <- ctors, n == name]
                xs2 = map (getType ren) xs
                
                f (Self, x) y = collapseSubtype x `unionPair` y
                f (FreeS i, x) (Subtype a b c d) = Subtype a b (c !!! (i, (c !! i) `unionPair` x)) d

        getType ren x | isVar x || isSel x = getExpr ren x
        
        getType ren (Call (CallFunc name) args) = getCall name typs
            where typs = map (getType ren) args


        getCall :: FuncName -> [Subtype] -> Subtype
        getCall "error" _ = Bot
        getCall name args = unionSubtype [x | Item n a x _ <- xs, n == name, and $ zipWith isSubset a args]


data Result = RVar Int | RBot | RTop | RCtor [String]
              deriving (Show, Eq)


fixpVariables :: [Item] -> Int -> [[Result]]
fixpVariables xs n = eqs
    where
        eqs = simpSets $ joinAllSets $ concatMap eqItem xs
    
        eqItem :: Item -> [[Result]]
        eqItem (Item _ _ a (Now b)) = eqSubtype a b
        eqItem _ = []
        
        eqSubtype :: Subtype -> Subtype -> [[Result]]
        eqSubtype (Subtype a1 b1 c1 d1) (Subtype a2 b2 c2 d2) = 
                eqUCtor a1 a2 ++ eqUCtor b1 b2 ++ f c1 c2 ++ f d1 d2
            where
                f x y = concat $ zipWith eqSubtype x y
        
        eqSubtype (SVar x) (SVar y) = [map RVar $ x ++ y]
        eqSubtype Empty _ = []
        eqSubtype _ Empty = []
        eqSubtype Top (SVar x) = [RTop : map RVar x]
        eqSubtype (SVar x) Top = [RTop : map RVar x]
        eqSubtype Bot (SVar x) = [RBot : map RVar x]
        eqSubtype (SVar x) Bot = [RBot : map RVar x]
        
    
        eqUCtor :: [UCtor] -> [UCtor] -> [[Result]]
        eqUCtor x y = [map f (x ++ y)]
            where
                f (UCtor x) = RCtor [x]
                f (UVar x) = RVar x
    
        simpSets :: [[Result]] -> [[Result]]
        simpSets x = concatMap f x
            where
                f [] = []
                f x = [nub x]
    
    
        joinAllSets :: [[Result]] -> [[Result]]
        joinAllSets x = f 0 x
            where
                f i x | i == n = x
                      | otherwise = f (i+1) (joinSets x i)
        
        joinSets :: [[Result]] -> Int -> [[Result]]
        joinSets rews n = concat a : b
            where (a,b) = partition (RVar n `elem`) rews
    

mapId :: (a -> Int -> (Int, b)) -> [a] -> Int -> (Int, [b])
mapId f [] n = (n,[])
mapId f (x:xs) n = (n3, x2:x3)
    where
        (n2,x2) = f x n
        (n3,x3) = mapId f xs n2



getSubtypeFree :: DataM SmallT -> LargeT -> Int -> (Int, Subtype)
getSubtypeFree datam (FreeL i) n = (n+1, SVar [n])
getSubtypeFree datam (CtorL name xs) n
        | recursive = let (n1,a1,b1) = f n
                          (n2,a2,b2) = f n1
                      in (n2, Subtype a1 a2 b1 b2)
        | otherwise = let (n1,a1,b1) = f n
                      in (n1, Subtype a1 [] b1 [])
    where
        f n = (a,[UVar n],b)
            where (a,b) = mapId (getSubtypeFree datam) xs (n+1)
    
        (DataT free ctors) = fromJust $ lookup name datam
        recursive = any isRecursive ctors
