
module Prepare.Firstify2(firstify2) where

import Yhc.Core hiding (collectAllVars,collectFreeVars,uniqueBoundVars,replaceFreeVars)
import Yhc.Core.FreeVar2

import Data.Maybe
import Data.List
import Data.Char
import Debug.Trace
import General.General

import Control.Monad.State
import qualified Data.Set as Set
import qualified Data.Map as Map


debugMode = False
debug msg x = if debugMode then trace msg x else x


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
firstify :: Core -> Result Core
firstify core = (if ans then success else failure) $
                coreReachable ["main"] $ fromCoreFuncMap core res
    where (ans,res) = transform $ prepare core


arity :: CoreFuncMap -> CoreFuncName -> Int
arity fm = length . coreFuncArgs . fromMaybe (error "arity") . coreFuncMapMaybe fm


---------------------------------------------------------------------
-- PREPARATION

prepare :: Core -> CoreFuncMap
prepare = lambdas . zeroApp . toCoreFuncMap . removeRecursiveLet . mapUnderCore remCorePos


-- insert explicit lambdas
lambdas :: CoreFuncMap -> CoreFuncMap
lambdas fm = Map.map (applyBodyFunc $ letInlineHO . mapUnderCore f) fm
    where
        f orig@(CoreApp (CoreFun name) args) | extra > 0 =
                CoreLam new (CoreApp (CoreFun name) (args ++ map CoreVar new))
            where
                extra = arity fm name - length args
                new = take extra $ freeVars 'v' \\ collectAllVars orig
        f x = x


-- make sure all applications are explicit
zeroApp :: CoreFuncMap -> CoreFuncMap
zeroApp = Map.map $ applyBodyFunc $ mapUnderCore f
    where
        f (CoreFun x) = CoreApp (CoreFun x) []
        f (CoreApp (CoreApp x ys) zs) = CoreApp x (ys++zs)
        f x = x

---------------------------------------------------------------------
-- TRANSFORMATION


type Template = (CoreFuncName,[CoreExpr])
data Spec = Spec {specId :: Int
                 ,specCore :: CoreFuncMap
                 ,specMap :: Map.Map Template CoreFuncName
                 -- the list of items which are currently under evaluation
                 ,specActive :: Set.Set CoreFuncName
                 -- those which were asked for while in active
                 ,specMissed :: Set.Set CoreFuncName
                 }

type SpecM a = State Spec a



transform :: CoreFuncMap -> (Bool, CoreFuncMap)
transform fm = evalState f newSpec
    where
        newSpec = Spec 1 fm Map.empty Set.empty Set.empty
        
        f = -- push down
            -- see if any in missed are lambdas, if so inline and repeat



-- functions that need to exist by the end
type Promise = (CoreFuncName, Template)






-- return True if you succeed, False if you fail
transform :: CoreFuncMap -> (Bool, CoreFuncMap)
transform fm = evalState (f 10 fm) newSpec
    where
        f 0 fm = return (False, fm)
        
        f n fm = do
            res <- specialise fm
            case inline res of
                Nothing -> return (True, res)
                Just res2 -> f (n-1) res2


isHO :: CoreExpr -> Bool
isHO (CoreLet _ x) = isHO x
isHO (CoreLam _ _) = True
isHO (CoreCase x ys) = any (isHO . snd) ys
isHO (CoreApp (CoreCon _) args) = any isHO args
isHO _ = False


---------------------------------------------------------------------
-- INLINE

-- return Nothing to say the transformation has finished (no higher order found)
-- return Just x to say that you did some inlining
inline :: CoreFuncMap -> Maybe CoreFuncMap
inline fm | Map.null inline = Nothing
          | otherwise = debug ("Inlining: " ++ concat (intersperse ", " $ Map.keys inline)) $
                        Just $ coreReachableMap ["main"] $
                               Map.map (applyBodyFunc $ letInlineHO . coreSimplifyExpr . mapUnderCore f) $
                               zeroApp fm
    where
        inline = Map.filter (isHO . coreFuncBody) fm
        
        f x@(CoreApp (CoreFun name) args) = case Map.lookup name inline of
                Nothing -> x
                Just func -> uncurry coreLam $ coreInlineFuncLambda func args
        f x = x


-- inline all let's which are higher order
letInlineHO :: CoreExpr -> CoreExpr
letInlineHO (CoreLet bind x) | not $ null ho = coreLet fo $ letInlineHO $ replaceFreeVars ho x
    where (ho,fo) = partition (isHO . snd) bind
letInlineHO x = setChildrenCore x $ map letInlineHO $ getChildrenCore x


---------------------------------------------------------------------
-- SPECIALISE


-- TYPES
-- the specialised data
type Template = (CoreFuncName,[CoreExpr])
data Spec = Spec {specId :: Int
                 ,specMap :: Map.Map Template CoreFuncName
                 ,specPromises :: [Promise]
                 }

-- functions that need to exist by the end
type Promise = (CoreFuncName, Template)


-- DRIVER OPERATIONS

newSpec = Spec 0 Map.empty []

specialise :: CoreFuncMap -> State Spec CoreFuncMap
specialise fm = f fm (Map.keys fm)
    where
        f :: CoreFuncMap -> [CoreFuncName] -> State Spec CoreFuncMap
        f fm [] = return fm
        f fm (x:xs) = do
            func <- specRequest fm (coreFuncMap fm x)
            s <- get
            put s{specPromises=[]}
            let fs = map (specFulfil fm) $ filter (not . (`Map.member` fm) . fst) $ nub $ reverse $ specPromises s
                fm2 = foldl (\m x -> Map.insert (coreFuncName x) x m) fm fs
            f (Map.insert x func fm2) (map coreFuncName fs ++ xs)



-- if we specialise this function, we will requires the
-- functions listed as promises to be generated
specRequest :: CoreFuncMap -> CoreFunc -> State Spec CoreFunc
specRequest fm func = do
    x <- mapUnderCoreM (specRequestExpr fm) (coreFuncBody func)
    return func{coreFuncBody=x}


specRequestExpr :: CoreFuncMap -> CoreExpr -> State Spec CoreExpr
specRequestExpr fm (CoreApp (CoreFun x) xs)
        | nxs >= ar && (nxs > ar || any isHO xs) = do
        let template = (x, map f xs)
        s <- get
        name <- case Map.lookup template (specMap s) of
            Just name -> do
                put s{specPromises = (name,template) : specPromises s}
                return name
            Nothing -> do
                let name = dropDollar x ++ "$" ++ show (specId s)
                put s{specId = specId s + 1
                     ,specMap = Map.insert template name (specMap s)
                     ,specPromises = (name,template) : specPromises s}
                return name
        s <- get
        return $ CoreApp (CoreFun name) (concatMap g xs)
    where
        nxs = length xs
        ar = arity fm x
        
        f x = if isHO x then normVars x else CoreVar "v1"
        g x = if isHO x then map CoreVar $ collectFreeVars x else [x]
        
        -- for all equivalent expressions
        -- irrelevant of free or bound var names, alpha rename to the same thing
        normVars x = normFree 'f' [] $ normBound 'b' [] $ normFree 'F' seen $ normBound 'B' seen x
            where seen = collectAllVars x

        normBound c seen x = runFreeVars $ do
            putVars (freeVars c \\ seen)
            uniqueBoundVars x
        
        normFree c seen x = replaceFreeVars
            (zip (collectFreeVars x) (map CoreVar $ freeVars c \\ seen))
            x
        
        dropDollar xs = if not (null nums) && not (null dol) then reverse (tail dol) else xs
            where (nums,dol) = span isDigit $ reverse xs

specRequestExpr _ x = return x



-- generate a function that uses a promise
specFulfil :: CoreFuncMap -> Promise -> CoreFunc
specFulfil fm (name,(func,args)) = 
        debug ("Specialise: " ++ name ++ " = " ++ show (CoreApp (CoreFun func) args)) $
        CoreFunc name (concat args2) (coreSimplifyExpr body2)
    where
        CoreFunc _ args1 body = fromMaybe (error "here") $ coreFuncMapMaybe fm func
        body2 = coreApp (replaceFreeVars (zip args1 params) body) (drop (length args1) params)

        (args2,params) = unzip $ f (freeVars 'v' \\ (args1 ++ collectAllVars body)) args
        
        f vars [] = []
        f vars (x:xs) = (used, replaceFreeVars (zip free (map CoreVar used)) x) : f left xs
            where
                (used,left) = splitAt (length free) $ vars \\ free
                free = collectFreeVars x
