
module Hill.Fusion(cmdsFusion) where

import Hill.Type
import Hill.Show
import Hill.Lets
import Hill.Simple
import Hill.Producer

import qualified Data.Map as Map
import Data.Map((!))
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe
import Data.List
import General.General
import Front.CmdLine
import System.IO


cmdsFusion = [Action "hill-fusion" fusion]


---------------------------------------------------------------------

fusion :: CmdLineState -> String -> Value -> IO Value
fusion state _ badHill = do
        hPutStrLn (cmdLineHandle state) $ showFuseTable fuseTable
        let (funcs2,items) = producer hill (funcs hill) (processor hill fuseTable) (generator hill fuseTable)
        hPutStrLn (cmdLineHandle state) $ showFuseItems items
        return $ ValueHill $ letLinearForm $ hill{funcs = funcs2}
    where
        hill = letNormalForm $ valueHill $ badHill
        fuseTable = calcFusion hill


type FuseTable = Map.Map FuncName Fusion

data Fusion = Fusion {produce :: Maybe DataName, consume :: Maybe (Int, DataName)}
              deriving Show


-- given this list of fused items, this is the fused version
type FuseItems = [([FuncName], FuncName)]


showFuseTable :: FuseTable -> String
showFuseTable x = unlines $ concatMap f $ Map.toList x
    where
        f (name, Fusion Nothing Nothing) = []
        f (name, fuse) = [name ++ " :: " ++ show fuse]


showFuseItems :: FuseItems -> String
showFuseItems xs = unlines [b ++ " = " ++ intercat " . " a | (a,b) <- xs]


calcFusion :: Hill -> FuseTable
calcFusion hill = Map.fromList $ map f (funcs hill)
    where
        f (Func name args body) = (name, Fusion (prod inner) (cons inner))
            where
                (binds,Var inner) = fromLet body
            
                -- getting a binding may be nothing
                -- if its a variable
                prod :: Int -> Maybe DataName
                prod i = do
                    res <- lookup i binds
                    case res of
                        Make x xs -> Just $ dataName $ getCtor hill x
                        Case on alts -> listToMaybe $ concatMap maybeToList res
                            where res = map (prod . fromVar . altExpr) alts
                        _ -> Nothing
                
                cons :: Int -> Maybe (Int, DataName)
                cons i = do
                    res <- lookup i binds
                    case res of
                        Case (Var on) alts | not $ null cs -> do
                            pos <- elemIndex on args
                            return (pos, dataName $ getCtor hill $ head cs)
                            where cs = [c | AltCtr c _ <- alts]
                        _ -> Nothing




processor :: Monad m => Hill -> FuseTable -> ([FuncName] -> m FuncName) -> Func -> m Func
processor hill fuseTable ask func =
    do
        binds2 <- mapM useFuse binds
        return func{body = mkLet binds2 (Var inner)}
    where
        -- nub since variables may be reached in more than one way
        lst = nub $ f inner
    
        (binds,Var inner) = fromLet $ body func
        
        useFuse (lhs,rhs) =
            case lookup lhs lst of
                Nothing -> return (lhs,rhs)
                Just (fs,args) -> do
                    name <- ask fs
                    return (lhs, Call name args)
        
        f i = case lookup i binds of
                  Nothing -> []
                  Just (Call x xs) ->
                      case fuseChain [] (Call x xs) of
                          Nothing -> concatMap (f . fromVar) xs
                          Just (fs,args) ->
                              [(i,(fs,args)) | length fs > 1] ++
                              concatMap (f . fromVar) args

                  Just x -> concatMap f [i | Var i <- allOverHill x]
        
        -- return a list of the functions in the chain
        -- their arguments which are't fused
        -- and the expression at the tail
        fuseChain :: [FuncName] -> Expr -> Maybe ([FuncName],[Expr])
        fuseChain seen (Call x xs) = do
                (pos,typc) <- consume (fuseTable ! x)
                Call y ys <- lookup (fromVar (xs !! pos)) binds
                typp <- produce (fuseTable ! y)
                let fuseargs = xs \!! pos
                if typc /= typp || y `elem` seen2
                    then Nothing
                    else Just $ case fuseChain seen2 (Call y ys) of
                            Nothing -> ([x,y], fuseargs ++ ys)
                            Just (fs,zs) -> (x:fs, fuseargs ++ zs)
            where
                seen2 = x:seen



generator :: Hill -> FuseTable -> [FuncName] -> Int -> Func
generator hill fuseTable names idn = res{funcName = newname}
    where
        res = f $ map (getFunc hill) $ reverse names
        newname = genUnique (intercat "_" names) idn
        
        f [x] = x
        f (p:c:xs) = f (pc:xs)
            where pc = merge c (fst $ fromJust $ consume $ fuseTable ! funcName c) p


        -- consumer, pos, producer
        merge :: Func -> Int -> Func -> Func
        merge consumer pos producer = letNormalFormFunc hill [] producer2
            where
                (binds, Var on) = fromLet $ body producer
            
                consumer2 = letNormalFormFunc hill (usedFree (body producer) ++ funcArgs producer) consumer
                
                producer2 = Func "" ((funcArgs consumer2 \!! pos) ++ funcArgs producer) (mkLet (map f binds) on2)
                    where
                        f (lhs,Case x alts) | lhs `elem` endset
                            = (lhs,Case x [alt{altExpr = g $ altExpr alt} | alt <- alts])
                        f x = x
                        
                        on2 = if null endset then g (Var on) else Var on
                        
                        g (Var i) | i `elem` endset = Var i
                                  | otherwise = case lookup i binds of
                            Just (Make x xs) ->
                                Let [(funcArgs consumer2 !! pos, Make x xs)] (body consumer2)
                            _ -> Call (funcName consumer2) (map Var (funcArgs consumer2) !!! (pos,Var i))
                
                -- all those case statements which are reached
                -- from the root
                endset = f on
                    where
                        f i = case lookup i binds of
                                  Just (Case on alts) -> i : concatMap (f . fromVar . altExpr) alts
                                  _ -> []


{-


data Store = Store {
        storeId :: Int,
        storeTable :: FuseItems
     }
     deriving Show


useFusion :: Hill -> FuseTable -> (FuseItems, [Func])
useFusion hill fuseTable = (storeTable b, a)
    where
        (a,b) = runState (mapM useFunc (funcs hill)) store
        store = Store (calcUnique hill) []


        useFunc func = do b <- useExpr (body func) ; return func{body = b}
        
        useExpr x@(Call prename _) | isJust fuse = do
                let Just (fs,xs) = fuse
                Store idn items <- get
                name <- case lookup fs items of
                    Just y -> return y
                    Nothing -> do
                        let newname = genUnique prename idn
                        put $ Store (idn + 1) ((fs,newname) : items)
                        return newname
                xs2 <- mapM useExpr xs
                return $ Call name xs2
            where
                fuse = fuseChain x
        
        useExpr x = do
            child <- mapM useExpr $ getChildren x
            return $ setChildren x child
        
        
        -- return a list of the functions in the chain
        -- their arguments which are't fused
        -- and the expression at the tail
        fuseChain :: Expr -> Maybe ([FuncName],[Expr])
        fuseChain (Call x xs) = do
                (pos,typc) <- consumer (fuseTable ! x)
                Call y ys <- getCall (xs !! pos)
                typp <- producer (fuseTable ! y)
                let fuseargs = [b | (a,b) <- zip [0..] xs, a /= pos]
                if typc /= typp
                    then Nothing
                    else Just $ case fuseChain (Call y ys) of
                            Nothing -> ([x,y], fuseargs ++ ys)
                            Just (fs,zs) -> (x:fs, fuseargs ++ zs)
            where
                getCall x@(Call _ _) = Just x
                getCall _ = Nothing
        
        



genFusion :: Hill -> FuseTable -> FuseItems -> [Func]
genFusion hill table items = map genFunc items
    where
        genFunc (comps,name) = f cons res{body = nubLets $ topLetsExpr $ body res}
            where
                res = (addLetsFunc $ getFunc hill prod){funcName = name}
            
                (prod:cons) = reverse comps
                
                f [] x = x
                f (c:cs) x = f cs (merge c x)
        
        merge cons prod = error $ show $ prod{body = simplify hill $ letInlineOnce $ letInlineSimp $ g $ body prod}
            where
                pos = fst $ fromJust $ consumer $ table ! cons
                posarg = funcArgs fun3 !! pos
            
                bod3 = body fun3
                fun3 = uniqueFunc (usedFree (body prod) ++ funcArgs prod) fun2
                bod2 = uniqueLetsExpr (usedFree bod) bod
                fun2 = fun{body = bod}
                bod = nubLets $ topLetsExpr $ body fun
                fun = addLetsFunc $ getFunc hill cons

                g (Let binds x) = Let binds (g x)
                g (Case on alts) = Case on [alt{altExpr = g $ altExpr alt} | alt <- alts]
                g (Make x xs) = Let [(posarg, Make x xs)] bod3
                g x = Let [(posarg, x)] bod3
                
                f (Let x y) = let (a,b) = f y in (x:a,b)
                f x = ([], x)
        
        
        uniqueFunc given func = func{funcArgs = args, body = bod}
            where
                args = take (length $ funcArgs func) $ [1..] \\ (given ++ usedFree (body func) ++ funcArgs func)
                bod = uniqueLetsExpr (args++given) $ replaceFree (zip (funcArgs func) (map Var args)) $ body func

-}
