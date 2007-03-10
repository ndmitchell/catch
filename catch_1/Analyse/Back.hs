
module Analyse.Back where



back :: Req Expr -> Prop (Req Expr)
back (Var   x         :< k) = on :< (c |> k)
    where Just (on, c) = var x
back (Make  c   xs    :< k) = replaceVars xs (c <| k)
back (Case  on  alts  :< k) = and [f c e | Alt c vs e <- alts]
    where f c e = (on :< notin c) || back (e :< k)
back (Call  fn  xs    :< k) = replaceVars xs (property fn k)

backs :: Prop Req -> Prop (Req VarName)
