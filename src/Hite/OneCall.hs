
-- Make sure there are no deeply nested calls
-- 2 level depth is required
-- Easy to do by introducing extra functions

module Hite.OneCall(cmd) where

import Hite.Type


cmd = cmdHitePure (const oneCall) "onecall"
            "Make sure there are no nested calls"


oneCall :: Hite -> Hite
oneCall hite = hite{funcs = concatMap makeOne (funcs hite)}
    where
    
makeOne :: Func -> [Func]
makeOne (Func name args body extra) = Func name args (MCase $ map fst res) extra : concatMap snd res
    where
        res = [g o | let MCase opts = body, o <- opts]

        g :: MCaseAlt -> (MCaseAlt, [Func])
        g (MCaseAlt p x) | callDepth x > 2 = (MCaseAlt p x, [])
                         | otherwise = (MCaseAlt p callon, makeOne (Func newname newargs newbody extra))
            where
                callon = Call (CallFunc newname) (map fst shallow ++ map Var args)
                newbody = MCase [MCaseAlt p (mapExpr rep x)]
                newname = name ++ "_one"
                newargs = map snd shallow ++ args
                shallow = zip [c | c@(Call _ _) <- allExpr x, callDepth c == 1] ["_one_" ++ show x | x <- [0..]]
                
                rep x = case lookup x shallow of
                            Nothing -> x
                            Just y -> Var y



callDepth :: Expr -> Int
callDepth x = maximum (0 : [(maximum $ map callDepth xs) + 1 | Call _ xs <- allExpr x])
