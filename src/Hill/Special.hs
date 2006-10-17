
module Hill.Special(cmdsSpecial) where

import Hill.Type
import Hill.Lambdas
import Hill.Show
import Hill.Simple
import Hill.Lets

import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad.Identity
import Data.Maybe
import General.General
import Front.CmdLine
import System.IO


cmdsSpecial = [Action "hill-special" special]


---------------------------------------------------------------------

special :: CmdLineState -> String -> ValueHill -> IO ValueHill
special state _ (ValueHill hill) = do
        let store = runStore hill
        hPutStrLn (cmdLineHandle state) $ showStoreTable store
        return $ ValueHill $ hill{funcs = storeCode store}


data Store = Store {
        storeId :: Int,
        storeTable :: Map.Map (FuncName,[Expr]) (FuncName,Expr),
        storeCode :: [Func]
     }
     deriving Show


showStoreTable :: Store -> String
showStoreTable x = unlines [show (Call oldname args) ++ " = " ++ newname ++ ", " ++ show res |
                            ((oldname,args),(newname,res)) <- Map.toList $ storeTable x]



runStore :: Hill -> Store
runStore hill = execState base (Store (calcUnique hill) Map.empty [])
    where
        base = ask "main" (replicate (length $ funcArgs $ getFunc hill "main") (Var 0))
        
        
        ask :: FuncName -> [Expr] -> State Store (FuncName, Expr)
        ask name args = do
            store <- get
            case Map.lookup (name,args) (storeTable store) of
                Nothing -> do
                    let func = getFunc hill name
                    newn <- if all (== Var 0) args
                            then return name
                            else do
                                put $ store{storeId = storeId store + 1}
                                return $ name ++ "_" ++ show (storeId store)
                    modify $ \store -> store{storeTable = Map.insert (name,args) (newn,Var 0) (storeTable store)}
                    res <- add newn func args
                    modify $ \store -> store{storeTable = Map.insert (name,args) (newn, res ) (storeTable store)}
                    return (newn,res)
                
                Just x -> return x

        
        add :: FuncName -> Func -> [Expr] -> State Store Expr
        add newname func args = do
                body4 <- {- liftM (simplify hill) $ -} alter body3
                let newfunc = Func newname [0..nargs-1] body4
                modify $ \store -> store{storeCode = newfunc : storeCode store}
                return $ makeAbstractRes hill body4
            where
                body3 = topLetsExpr $ addLetsExpr (funcArgs func) $ simplify hill body2
                body2 = replaceFree (zip (funcArgs func) args) $ body func
                (nargs,reps) = ascendingFrees args


        -- needs to go top down (since that guarantees maximum information)
        alter :: Expr -> State Store Expr
        alter (Let binds x) = do
                binds2 <- mapM (\(a,b) -> liftM ((,) a) (alter b)) binds

                let lhs = map fst binds2
                (keep,inline) <- mapAndUnzipM alterBind binds2
                
                x2 <- alter $ moveLambdas $ replaceFree (zip lhs inline) x
                return $ Let (zip lhs keep) x2
        
        alter x = do
            child <- mapM alter $ getChildren x
            return $ setChildren x child

        
        -- from an Expr, figure out what should stay here, and what should be inlined
        alterBind :: (Int, Expr) -> State Store (Expr, Expr)
        
        alterBind (n,Let binds x) = do
            (keep,inline) <- alterBind (n,x)
            return (Let binds keep, inline)
       
        alterBind (n,Call "ap%" [(Make x y),z]) = alterBind (n, res)
            where
                res = case alt of
                          Make q _ -> Make q (y ++ [z])
                          Call q _ -> Call q (y ++ [z])
            
                alt = altExpr $ head $ filter ((== ACtor x) . altVal) alts
                Case on alts = body $ getFunc hill "ap%"
        
        alterBind (n,Call x xs) = do
            let (abstract,concrete) = makeAbstractArgs hill xs
            (name,result) <- ask x abstract
            return (Call name concrete, makeConcreteRes result n)

        -- always inline constructor applications
        alterBind (n,orig) = return (orig, makeConcreteRes abstract n)
            where abstract = makeAbstractRes hill orig


-- take a list of argument, return a template and the concrete values
-- length xs == length (fst res)
-- length [Var 0 <- allExpr (snd res)] == length (snd res)
makeAbstractArgs :: Hill -> [Expr] -> ([Expr], [Expr])
makeAbstractArgs hill xs = fs (repeat True) xs
    where
        fs enable xs = (as, concat bs)
            where (as,bs) = unzip $ zipWith f enable xs
        
        f True (Make x xs) = (Make x as, bs)
            where
                (as,bs) = fs notRecs xs
                notRecs = if null $ types $ getCtor hill x
                          then repeat True
                          else [not $ cargRec $ getCArgPos hill x i | i <- [0..length xs-1]]

        f _ (Const x) = (Const x, [])
        f _ x = (Var 0, [x])



makeConcreteRes :: Expr -> Int -> Expr
makeConcreteRes x n = mapOverHill f x
    where
        f (Var 0) = Var n
        f x = x


-- make a result "more abstract"
-- starts with the code
-- : a b ==> : @0.hd @0.tl
makeAbstractRes :: Hill -> Expr -> Expr
makeAbstractRes hill x = f (Var 0) x
    where
        f var (Let _ x) = f var x
        f var (Const x) = Const x
        f var (Make x xs) = Make x (zipWith (\c x -> f (Sel var c) x) cs xs)
            where cs = ctorArgs $ getCtor hill x
        f var _ = var



-- replace the free variables Var 0, with Var 0..Var n
ascendingFrees :: [Expr] -> (Int, [Expr])
ascendingFrees xs = fs xs 0
    where
        f :: Expr -> Int -> (Int, Expr)
        f (Var _) n = (n+1, Var n)
        f x n = (n2, setChildren x childs)
            where (n2, childs) = fs (getChildren x) n
        
        
        fs :: [Expr] -> Int -> (Int, [Expr])
        fs [] n = (n, [])
        fs (x:xs) n = (n3, x2:xs3)
            where
                (n2,x2) = f x n
                (n3,xs3) = fs xs n2
