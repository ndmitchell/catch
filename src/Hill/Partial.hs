
module Hill.Partial(cmdsPartial) where

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

import Debug.Trace

cmdsPartial = [Action "hill-partial" partial]


---------------------------------------------------------------------

partial :: CmdLineState -> String -> Value -> IO Value
partial state _ hillBad = error $ show $ runStore hill
        --hPutStrLn (cmdLineHandle state) $ showTemplate template
        --return $ ValueHill $ makeCode hill template
    where
        hill = moveLambdas $ addLambdas $ topLets $ addLets $ applyFuns $ valueHill hillBad
        --template = filterTemplate $ makeTemplate hill


data Store = Store {
        storeId :: Int,
        storeTable :: Map.Map (FuncName,[Expr]) (FuncName,Expr),
        storeCode :: [Func]
     }
     deriving Show


runStore :: Hill -> Store
runStore hill = execState base (Store 1 Map.empty [])
    where
        base = ask "main" (replicate (length $ funcArgs $ getFunc hill "main") (Var 0))
        
        
        ask :: FuncName -> [Expr] -> State Store (FuncName, Expr)
        ask name args = do
            () <- trace ("ask " ++ name ++ " " ++ show args) (return ())
            store <- get
            case Map.lookup (name,args) (storeTable store) of
                Nothing -> do
                    let func = getFunc hill name
                    newn <- if all (== Var 0) args && length args == length (funcArgs func)
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
                body3 = moveLambdas $ addLambdasExpr hill $ topLetsExpr $ addLetsExpr (funcArgs func) $
                        useVectorMake $ applyFuns $ simplify hill body2
                body2 = mkApply (replaceFree (zip (funcArgs func) norm) $ body func) super
                
                (norm,super) = splitAt (length fargs) reps
                fargs = funcArgs func
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
        
        alterBind (n,Apply (Fun x) xs) = do
            let (abstract,concrete) = makeAbstractArgs xs
            (name,result) <- ask x abstract
            return (Apply (Fun name) concrete, makeConcreteRes result n)

        -- always inline constructor applications
        alterBind (n,orig) = return (orig, makeConcreteRes abstract n)
            where abstract = makeAbstractRes hill orig


-- take a list of argument, return a template and the concrete values
-- length xs == length (fst res)
-- length [Var 0 <- allExpr (snd res)] == length (snd res)
makeAbstractArgs :: [Expr] -> ([Expr], [Expr])
makeAbstractArgs xs = fs xs
    where
        fs xs = (as, concat bs)
            where (as,bs) = unzip $ map f xs
        
        f (Make x xs) = (Make x as, bs)
            where (as,bs) = fs xs
        f (Lambda n (Apply (Fun x) xs)) = (Lambda n (Apply (Fun x) as), bs)
            where (as,bs) = fs xs
        f (Const x) = (Const x, [])
        f (Lambda n x) = (Lambda n (Var 0), [x])
        f x = (Var 0, [x])



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
        f var (Lambda n (Apply (Fun x) [])) = Lambda n (Apply (Fun x) [])
        f var (Lambda n _) = Lambda n var
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

