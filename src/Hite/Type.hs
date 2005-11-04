
{-|
    A signature for the abstract type
-}

module Hite.Type where


-- constraints:
-- Data.dataName is ignored for most purposes
-- unique(datas.*ctors.*args)
-- unique(datas.*funcs.funcName)
-- unique(datas.funcs.*args)

type FuncName = String
type DataName = String
type CtorName = String
type CtorArg  = String
type FuncArg  = String

data Hite = Hite {datas :: [Data], funcs :: [Func]}

data Data = Data {dataName :: DataName, ctors :: [Ctor]}
data Ctor = Ctor {ctorName :: CtorName, ctorArgs :: [CtorArg]}

data Func = Func {funcName :: FuncName, funcArgs :: [FuncArg], expr :: Expr}


data Expr = Call {callFunc :: Expr, callArgs :: [Expr]}
          | Var FuncArg [CtorArg] -- variable, the path you have to go down
                                 -- all variables are parameter based, 1 indexed
          | CallFunc {callName :: FuncName}
          | Make {makeName :: CtorName, makeArgs :: [Expr]}
          | Case Expr [(CtorName, Expr)] -- case x of Cons a b, Nil -> Case "x" (Cons, ["a", "b"]), (Nil, [])




{- 
getFunc :: Hite -> FuncName -> Exp
getFunc h s = funcs h ! s


type Data = [[CtorName]]

data Sel = Sel {selName :: CtorName, selNum :: Int}
           deriving (Eq)


data Exp = Call {callFunc :: Exp, callArgs :: [Exp]}
         | Var Int [Sel] -- variable, the path you have to go down
                         -- all variables are parameter based, 1 indexed
         | CallFunc {callName :: String}
         | Make {makeName :: CtorName, makeArgs :: [Exp]}
         | Case Exp [(CtorName, Exp)] -- case x of Cons a b, Nil -> Case "x" (Cons, ["a", "b"]), (Nil, [])
         
         -- arggghh, the world just became higher order
         | CallHO Exp [Exp]
           deriving (Show, Eq)


-- HITE MANIPULATION FUNCTIONS

instance Show Sel where
    show (Sel a n) | a == "Cons" || a == ":" =
                   case n of
                       1 -> "head"
                       2 -> "tail"
                       x -> "(ERROR Cons:" ++ show x ++ ")"
    
    show (Sel a b) = a ++ "#" ++ show b


varAdd :: Exp -> String -> Int -> Exp
varAdd (Var n xs) str int = Var n (xs ++ [Sel str int])
varAdd x _ _ = error $ "varAdd: Exp not a variable (" ++ show x ++ ")"


-- given one constructor, list all the constructors of that type
allCtors :: Hite -> CtorName -> [CtorName]
allCtors hite ctor = concat $ filter (ctor `elem`) (datas hite)


isCase (Case _ _) = True ; isCase _ = False
isCall (Call _ _) = True ; isCall _ = False
isVar (Var _ _) = True ; isVar _ = False
isMake (Make _ _) = True ; isMake _ = False


allExps :: Exp -> [Exp]
allExps x = x : concatMap allExps (case x of
        Call x xs -> x : xs
        Make _ xs -> xs
        Case _ xs -> snds xs
        _ -> []
    )
    
allExpsFunc hite func = allExps (getFunc hite func)


mapExp :: (Exp -> Exp) -> Exp -> Exp
mapExp f x = f $ case x of
    Call a bs -> Call (mapExp f a) (map (mapExp f) bs)
    Make a bs -> Make a            (map (mapExp f) bs)
    Case a bs -> Case (mapExp f a) (map (\(d,e) -> (d,mapExp f e)) bs)
    _ -> x
    

isCaseComplete :: Hite -> Exp -> Bool
isCaseComplete hite (Case _ stmts) = null (allCtors hite (head ctors) \\ ctors)
    where ctors = fsts stmts



listHigherOrder :: Hite -> [String]
listHigherOrder hite = map fst (filter f (funcs hite))
    where
        f (name, expr) = any g (allExps expr)
        g (Call (CallFunc _) _) = False
        g (Call _ _) = True
        g _ = False



-- represent call's which have got dropped to one end
callLift :: Hite -> Hite
callLift hite = hite{funcs = map (\(a,b) -> (a, mapExp f b)) (funcs hite)}
    where
        f (Call (Call a b) c) = Call a (b++c)
        f x = x

-}
