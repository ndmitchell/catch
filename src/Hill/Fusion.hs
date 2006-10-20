
module Hill.Fusion(cmdsFusion) where

import Hill.Type
import Hill.Show
import Hill.Lets
import Hill.Simple

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

fusion :: CmdLineState -> String -> Hill -> IO Hill
fusion state _ hill = do
        hPutStrLn (cmdLineHandle state) $ showFuseTable fuseTable
        let (items, funcs2) = useFusion hill fuseTable
        hPutStrLn (cmdLineHandle state) $ showFuseItems items
        return $ hill{funcs = funcs2 ++ genFusion hill fuseTable items}
    where
        fuseTable = calcFusion hill


type FuseTable = Map.Map FuncName Fusion

data Fusion = Fusion {producer :: Maybe DataName, consumer :: Maybe (Int, DataName)}
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
        f (Func name args body) = (name, Fusion (produce body) (consume body))
            where
            
                produce :: Expr -> Maybe DataName
                produce (Make x xs) = Just $ dataName $ getCtor hill x
                produce (Let _ x) = produce x
                produce (Case on alts) = listToMaybe $ concatMap maybeToList res
                    where res = map (produce . altExpr) alts
                produce x = Nothing
                

                consume :: Expr -> Maybe (Int, DataName)
                consume (Case (Var on) alts) | not $ null cs
                        = do pos <- elemIndex on args
                             return (pos, dataName $ getCtor hill $ head cs)
                    where
                        cs = [c | AltCtr c _ <- alts]

                consume (Let _ x) = consume x
                consume _ = Nothing



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

