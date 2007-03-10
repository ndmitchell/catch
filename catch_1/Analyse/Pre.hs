
module Analyse.Pre where



pre :: Expr -> Prop (Req Expr)
pre (Var   x         ) = True
pre (Make  _   xs    ) = and (map pre xs)
pre (Call  fn  xs    ) = preFunc fn xs && and (map pre xs)
pre (Case  on  alts  ) = pre on && and [f c e | Alt c vs e <- alts]
    where f c e = (on :< notin c) || pre e
