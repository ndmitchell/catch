
module Hite.DeadArgs(deadArgs) where

import Hite.Type

-- potentionally unsafe if unsaturated applications exist

deadArgs :: Hite -> Hite
deadArgs hite = error $ show $ findDead hite




findDead :: Hite -> [(FuncName, [Int])]
findDead hite = filter (not . null . snd) $ map f $ funcs hite
    where
        f (Func name args body _) = (name, [n | (n,a) <- zip [0..] args, not (isDead a body)])
        isDead arg body = or [n == arg | Var n _ <- allExpr body]
