
module Analyse.Core where


ctors        :: CtorName  -> [CtorName]
arity        :: CtorName  -> Int
var          :: VarName   -> Maybe (Expr, Field)
instantiate  :: FuncName  -> [Expr] -> Expr
isRec        :: Field     -> Bool
