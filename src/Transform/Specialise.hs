
module Transform.Specialise where

import Transform.Type
import General.General
import Data.List
import Control.Exception


specialise :: IHite -> IHite
specialise ihite = decodeCell $ reach $ useNames $ allocNames $ useSpec $ drive ihite



eqFuncs (IHite _ a1) (IHite _ a2) = a1 == a2
        
-- fixed point on all operations
-- apart from the cleanup ones
drive :: IHite -> IHite
drive x = f (simpler x)
    where
        -- the x /= x2 test should not be needed
        -- but it is for Risers at the very least
        -- probably lurking bug...
        f x = if b || not (x `eqFuncs` x2) then f x2 else x2
            where (b,x2) = oneStep (False,x)
    
        oneStep = liftId simpler . liftMay inliner .
                  liftId simpler . liftMay genSpec . liftId useSpec .
                  liftId simpler . liftMay arityRaise
    
        liftId f (b, x) = (b, f x)
        liftMay f (b, x) = case f x of {Nothing -> (b, x); Just y -> (True, y)}



applyAll :: (IExpr -> IExpr) -> IHite -> IHite
applyAll f (IHite a b) = IHite a [func{funcExpr = mapOver f (funcExpr func)} | func <- b]


getFunc2 :: IHite -> FuncPtr -> IFunc
getFunc2 (IHite _ funcs) (FuncPtr a b) = headNote ("getFunc2: Cant find: " ++ show (FuncPtr a b)) res
    where res = [func | func@(Func _ _ _ [(TweakExpr c,d)]) <- funcs, c == b && d == a]


decodeCell :: IHite -> IHite
decodeCell ihite = applyAll f ihite
    where
        f (Cell (FuncPtr name _) 0 args) = Call name args
        f x = x


reach :: IHite -> IHite
reach ihite@(IHite a b) = IHite a $ filter (\x -> funcName x `elem` keep) b
    where
        keep = fixSet f ["main"]
        
        f x = nub [x | Cell (FuncPtr x _) _ _ <- allOver $ funcExpr $ getFunc ihite x]



allocNames :: IHite -> IHite
allocNames (IHite a b) = IHite a (zipWith f [1..] b)
    where
        f n func | last nam == '?' = func{funcName = init nam ++ "~" ++ show n}
                 | otherwise = func
            where nam = funcName func


useNames :: IHite -> IHite
useNames ihite@(IHite _ funcs) = applyAll f ihite
    where
        f (Cell name n xs) = Cell (FuncPtr (funcName $ getFunc2 ihite name) []) n xs
        f x = x
        


-- find a fixed point always
-- return Nothing if no changes
inliner :: IHite -> Maybe IHite
inliner ihite = if res `eqFuncs` ihite then Nothing else Just res
    where
        res = applyAll f ihite
    
        f (Cell name 0 xs) | canInline func = mapOver f $ mapOver g (funcExpr func)
            where
                func = getFunc2 ihite name 
                g (Var i) = xs !! i
                g x = x
        f x = x

        canInline (Func name _ body [(TweakExpr a,b)]) =
            case body of
                (Cell nam n xs) | nam /= name && all isSimple xs -> True
                -- (Cell nam n xs) | n /= 0 && nam /= name && all isReasonable xs -> True
                (Make ('(':_) xs) -> True
                _ -> isSimple body
            where name = FuncPtr b a
        
        isSimple (Var x) = True
        isSimple (Sel x y) = isSimple x
        isSimple x = False
        
        isReasonable (Cell nam 0 xs) = all isSimple xs
        isReasonable x = isSimple x


-- perform basic arity raising, makes it no longer required to inline Cell's harder
arityRaise :: IHite -> Maybe IHite
arityRaise ihite@(IHite datas funcs) =
        if null raisers then Nothing else
        case arityRaise res of
            Nothing -> Just res
            x -> x
    where
        res = IHite datas2 (newfuncs++funcs2)
        ihite2@(IHite datas2 funcs2) = applyAll f ihite
        newfuncs = map g raisers
        
        done = [FuncPtr b a | Func _ _ _ [(TweakExpr a,b)] <- funcs]
    
        raisers = concatMap chooseRaise funcs
        chooseRaise (Func name _ body [(TweakExpr a,b)]) =
            [(FuncPtr b a,i) | let i = getArity body, i /= 0, let newptr = FuncPtr b (a ++ replicate i (Var 0)),
                               not (newptr `elem` done)]
        
        getArity (Cell _ n _) = n
        getArity (Case on alts) = maximum $ map (getArity . snd) alts
        getArity _ = 0
        
        g (ptr, i) = Func (name ++ "?") (args ++ newargs) (Apply body (map Var newargs)) [(TweakExpr (a ++ zeros newargs),b)]
            where
                newargs = take i $ [0..] \\ args
                Func name args body [(TweakExpr a, b)] = getFunc2 ihite2 ptr


        f orig@(Cell on@(FuncPtr a b) n xs) = case lookup on raisers of
            Nothing -> orig
            Just i -> Cell (FuncPtr a (b ++ replicate i (Var 0))) (n+i) xs
        f x = x



-- simple transforms, are each individual
-- do not depend on the syntactic info in the rest of the program!
-- guarantee to find a fixed point in one single iteration
simpler :: IHite -> IHite
simpler ihite = applyAll f ihite
    where
        -- simple syntax
        f (Apply (Cell x n xs) (y:ys)) | n >= 1 = f $ Apply (Cell x (n-1) (xs++[y])) ys
        f (Apply x []) = x
    
        -- more involved
        -- (Cons a b).head ==> a
        -- Nil.head ==> (no change) - may be in a redundant branch
        f (Sel (Make name args) arg) | ctorName x == name = args !! cargPos x
            where x = getCArg ihite arg

        -- case Nil of {Nil -> a; Cons -> b} ==> a
        f (Case (Make name args) xs) = headNote "Tranform.Rewrite.basicExpr" ys
            where ys = [b | (a,b) <- xs, a == name || null a]

        -- Apply (case {a -> b; c -> d}) x ==> case {a -> Apply b x; c -> Apply d x}
        f (Apply (Case on alts) x) = Case on [(a,f $ Apply b x) | (a,b) <- alts]

        f x = x
        


-- only return Just if you actually did something
-- needs to fixpoint because there may be a specialised instance
-- that does not have any concrete implementation - i.e. was inlined
-- itself!
genSpec :: IHite -> Maybe IHite
genSpec ihite = f (genSpecOne ihite)
    where
        f Nothing = Nothing
        f (Just x) = Just $ g x
        
        g x = case genSpecOne x of
                  Nothing -> x
                  Just y -> g y
        

genSpecOne :: IHite -> Maybe IHite
genSpecOne ihite@(IHite datas funcs) = if null newFuncs then Nothing else Just res
    where
        res = IHite datas (map f newFuncs ++ funcs)
        newFuncs = nub reqFuncs \\ haveFuncs
        haveFuncs = [FuncPtr b a | func <- funcs, let [(TweakExpr a,b)] = funcTweaks func]
        reqFuncs = [x | func <- funcs, Cell x _ _ <- allOver (funcExpr func)]
        
        f (FuncPtr name args) = res
            where
                res = Func (name ++ "?") [0..argCount-1] body [(TweakExpr args,name)]
            
                Func _ origArgs origBody _ = getFunc ihite name
                (argCount, args2) = giveNumbers args
                body = Apply (mapOver g origBody) (drop (length origArgs) args2)
                
                g (Var i) = args2 !! i
                g x = x



-- automatically finds a fixed point
useSpec :: IHite -> IHite
useSpec ihite = applyAll f ihite
    where
        f (Apply (Cell x 0 args) (e:xtra)) = if null xtra then res else f $ Apply res xtra
            where res = Cell (addArg x) 0 (args++[e])
        
        f cell@(Cell{}) = specFunc ihite cell
        
        f x = x



addArg :: FuncPtr -> FuncPtr
addArg (FuncPtr name args) = FuncPtr name (args ++ [Var 0])
--    where fresh = maximum $ 0:[a+1 | arg <- args, Var a <- allOver arg]


zeros = map (const $ Var 0)

specFunc ihite orig@(Cell (FuncPtr name specs) n args) = res
    where
        res = Cell (FuncPtr name (applySpec newspecs)) n (concat newargs)
        (newspecs, newargs) = unzip
            [if valid then (a,b) else (Var 0, [x]) |
                (pre,x,post) <- allItems args,
                let (a,b) = howSpec x,
                let valid = checkSpec $ applySpec $ zeros pre ++ [a] ++ zeros post]
    
        -- how would you like to specialise
        howSpec :: IExpr -> (IExpr, [IExpr])
        howSpec (Make n ys) = (Make n (map (const $ Var 0) ys), ys)
        howSpec (Cell nam n xs) | n /= 0 = (Cell nam n (map (const $ Var 0) xs), xs)
        howSpec x = (Var 0, [x])
    
    
        -- now weave the specialisations back in
        applySpec :: [IExpr] -> [IExpr]
        applySpec newspecs =
                assertNote (show (orig,specs,n2,x2,ys)) (n2 == length ys) $
                map (mapOver g2) x2
            where
                ys = newspecs ++ replicate n (Var 0)
                (n2,x2) = giveNumbers specs
                
                g2 (Var i) = ys !! i
                g2 x = x

        -- check specialisation
        checkSpec :: [IExpr] -> Bool
        checkSpec x = all (all f . allOver) x
            where
                f (Make x xs) = and [not $ cargRec $ getCArgPos ihite x i
                                    | (i,Make x2 _) <- zip [0..] xs, x2 == x]
                f _ = True



-- replace all Var 0's with Var 0..n
-- return the count (i.e. the next free)
giveNumbers :: [IExpr] -> (Int, [IExpr])
giveNumbers xs = f1 xs 0
    where
        f1 (x:xs) n = (c, b:d)
            where
                (a,b) = f2 x n
                (c,d) = f1 xs a
        f1 [] n = (n, [])
        
        f2 (Var 0) n = (n+1, Var n)
        f2 x n = (a, setChildren x b)
            where (a,b) = f1 (getChildren x) n
