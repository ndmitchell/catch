
module Prepare.Firstify2(firstify2) where

import Yhc.Core hiding (collectAllVars,collectFreeVars,uniqueBoundVars,replaceFreeVars)
import Yhc.Core.FreeVar2

import Prepare.Firstify.Template
import Prepare.Firstify.Prepare

import Data.List
import Data.Char
import Data.Maybe
import General.General

import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map

{-
SPECIALISE ALGORITHM

Need to generate a specialised version if:
* f gets called with more arguments than its arity
* any argument is higher order

The specialised version has:
* a free variable for each non-ho argument
* the free variables within a function, for a ho argument
-}



---------------------------------------------------------------------
-- DRIVER

-- if the first result is not null, an error occurred
-- the second result is how far you got
firstify2 :: Core -> Result Core
firstify2 core = (if ans then success else failure) $
                coreReachable ["main"] $ fromCoreFuncMap core res
    where (ans,res) = transform $ prepare core



data Spec = Spec {specId :: Int
                 ,specCore :: CoreFuncMap
                 ,specMap :: Map.Map (CoreFuncName,Template) CoreFuncName
                 -- the list of items which are currently under evaluation
                 ,specActive :: Set.Set CoreFuncName
                 -- the functions which got finished
                 ,specDone :: Set.Set CoreFuncName
                 -- those which were asked for while in active
                 ,specMissed :: Set.Set CoreFuncName
                 }

type SpecM a = State Spec a



transform :: CoreFuncMap -> (Bool, CoreFuncMap)
transform fm = (True, evalState f newSpec)
    where
        newSpec = Spec 1 fm Map.empty Set.empty Set.empty Set.empty
        
        mainArgs = take (length $ coreFuncArgs $ coreFuncMap fm "main") $ freeVars 'v'
        
        f = do lam (CoreApp (CoreFun "main") (map CoreVar mainArgs))
               s <- get
               if any (isHO . coreFuncBody . coreFuncMap (specCore s)) $ Set.toList (specMissed s)
                   then put s{specActive=Set.empty, specDone=Set.empty, specMissed=Set.empty} >> f
                   else return $ specCore s



lam :: CoreExpr -> SpecM CoreExpr
lam (CoreApp (CoreFun f) xs) = do
    xs <- mapM lam xs
    s <- get
    let func = coreFuncMap (specCore s) f

    -- make sure that the templating is done
    (f,xs) <- case useTemplate func xs of
        Nothing -> return (f,xs)
        Just (template,args) -> do
            s <- get
            case Map.lookup (f,template) (specMap s) of
                Just f2 -> return (f2,args)
                Nothing -> do
                    let newname = dropDollar f ++ "$" ++ show (specId s)
                    put s{specId = specId s + 1
                         ,specCore = Map.insert newname (genTemplate template func newname) (specCore s)
                         ,specMap = Map.insert (f,template) newname (specMap s)}
                    return (newname,args)

    -- now try and do the transformation on it if required
    when (not (Set.member f (specDone s)) && not (Set.member f (specActive s))) $ do
        s <- get
        put s{specActive = Set.insert f (specActive s)}
        let func = coreFuncMap (specCore s) f
        res <- lam (coreFuncBody func)
        modify $ \s -> s{specCore = Map.insert f func{coreFuncBody = res} (specCore s)
                        ,specActive = Set.delete f (specActive s)
                        ,specDone = Set.insert f (specDone s)}

    -- now inline the function if required
    s <- get
    when (not $ Set.member f $ specDone s) $ put s{specMissed = Set.insert f (specMissed s)}
    let func = coreFuncMap (specCore s) f
    if not $ isHO $ coreFuncBody func then
        return $ CoreApp (CoreFun f) xs
     else
        lam (fromJust $ coreInlineFunc func xs)

lam (CoreApp (CoreVar x) xs)  = liftM (CoreApp (CoreVar  x)) (mapM lam xs)
lam (CoreApp (CoreCon  x) xs) = liftM (CoreApp (CoreCon  x)) (mapM lam xs)
lam (CoreApp (CorePrim x) xs) = liftM (CoreApp (CorePrim x)) (mapM lam xs)

lam (CoreApp (CoreLam xs body) ys) =
        lam $ coreApp (coreLam (drop n xs) (replaceFreeVars (zip xs ys) body)) (drop n ys)
    where n = min (length xs) (length ys)

lam (CoreApp (CoreCase on alts) xs) = lam $ CoreCase on [(a, CoreApp b xs) | (a,b) <- alts]

lam (CoreApp (CoreApp x ys) zs) = lam $ CoreApp x (ys++zs)

lam (CoreLet binds x) = do
    rhs <- mapM (lam . snd) binds
    let (ho,fo) = partition (isHO . snd) (zip (map fst binds) rhs)
    x2 <- lam $ replaceFreeVars ho x
    return $ CoreLet fo x2

lam (CoreCase on alts) = do
    on2 <- lam on
    case on2 of
        CoreApp (CoreCon c) xs ->
            lam $ head $ 
                 [replaceFreeVars (zip (map fromCoreVar xs2) xs) rhs
                    | (CoreApp (CoreCon c2) xs2, rhs) <- alts, c2 == c] ++
                 [replaceFreeVars [(lhs,on2)] rhs | (CoreVar lhs,rhs) <- alts]
        _ -> do
            rhs <- mapM (lam . snd) alts
            return $ CoreCase on2 (zip (map fst alts) rhs)

lam x | isCoreLam x || isCoreVar x || isCoreConst x = return x

lam x = error $ show x



dropDollar xs = if not (null nums) && not (null dol) then reverse (tail dol) else xs
    where (nums,dol) = span isDigit $ reverse xs

