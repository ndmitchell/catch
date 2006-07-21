
module Typey.Evaluate(evaluate) where

import Typey.Type
import Typey.Abstract
import Hite
import General.General


type Env = (Hite, DataM SmallT, Func2M)


evaluate :: (String -> IO ()) -> Hite -> DataM SmallT -> Func2M -> [Abstract] -> IO Abstract
evaluate logger hite datam funcm args = return $ eval env (zip arg args) body
    where
        env = (hite, datam, funcm)
        Func _ arg body _ = getFunc hite "main"




eval :: Env -> [(String, Abstract)] -> Expr -> Abstract
eval env@(hite,datam,funcm) args expr =
    case expr of
        Call (CallFunc name) params -> eval env (zip arg args2) body
            where
                Func _ arg body _ = getFunc hite name
                args2 = map (eval env args) params
        
        Case x alts -> unionAbs $ concatMap f alts
            where
                x2 = eval env args x
                f (opt,expr) = if hasCtorAbs datam x2 opt then [eval env args expr] else []
        
        Var x -> lookupJust x args
        
        Sel x y -> followSelAbs datam (eval env args x) y
        
        Error _ -> AbsBottom
        
        x -> error $ "eval: " ++ output x
