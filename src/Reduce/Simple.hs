
{-|
    Reduce from simple Haskell, to Hite
-}

module Reduce.Simple(reduceSimple) where

import Haskell
import Hite
import Char
import General
import Maybe


reduceSimple :: Haskell -> Hite
reduceSimple (HsModule _ _ _ _ decls) = 
        hite{funcs=[redFunc hite x | x@(HsFunBind {}) <- decls]}
    where
        hite = Hite [redData x | x@(HsDataDecl{}) <- decls] []


-- lots of things are essentially identifiers, but "wrapped up"
-- have a class so I don't need to know the types

class Ident a where ; ident :: a -> String

instance Ident HsName  where ; ident (HsIdent x) = x
instance Ident HsQName where ; ident (UnQual  x) = ident x
instance Ident HsPat   where ; ident (HsPVar  x) = ident x


redData :: HsDecl -> Data
redData x@(HsDataDecl _ _ name polyTypes cons _) = Data (ident name) (map redCtor cons)


redCtor :: HsConDecl -> Ctor
redCtor (HsConDecl _ name params) = Ctor (ident name) (map namePrefix [1..length params])
    where
        namePrefix x = toLower n : ame ++ "#" ++ show x
        (n:ame) = ident name

redCtor (HsRecDecl _ name params) = Ctor (ident name) (concatMap (map ident . fst) params)



redFunc :: Hite -> HsDecl -> Func
redFunc hite (HsFunBind [HsMatch _ name vars (HsUnGuardedRhs body) []]) = 
        Func (ident name) params (redExpr hite [(x, Var x []) |x <- params] body)
    where
        params = map ident vars


redExpr :: Hite -> [(String, Expr)] -> HsExp -> Expr
redExpr hite rep (HsCase (HsVar var) alts) =
        Case on (map redAlt alts)
    where
        on@(Var name path) = fromJust $ lookup (ident var) rep
    
        redAlt :: HsAlt -> (String, Expr)
        redAlt (HsAlt _ (HsPApp con vars) (HsUnGuardedAlt ex) []) =
                (ctor, redExpr hite (rep ++ newRep)  ex)
            where
                ctor = ident con
                newRep = [(ident v, Var name (path++[p])) |
                          (v,p) <- zip vars (ctorArgs $ getCtor ctor hite)]

redExpr hite rep (HsVar x) = lookupDef (CallFunc qx) qx rep
    where qx = ident x



redExpr hite rep (HsApp f ex) = case f' of
       Make name args -> Make name (args ++ [ex'])
       Call name args -> Call name (args ++ [ex'])
       x -> Call x [ex']
   where
       f'  = redExpr hite rep f
       ex' = redExpr hite rep ex


redExpr hite rep (HsCon x) = Make (ident x) []

redExpr hite rep (HsParen x) = redExpr hite rep x


-- literals
redExpr hite rep (HsLit (HsString [])) = Make "Nil" []
redExpr hite rep (HsLit (HsString (x:xs))) = Make "Cons" [Make "Char" [], res]
    where res = redExpr hite rep (HsLit (HsString xs))



redExpr hite rep x = error $ "Haskell2Hite.exp2hite: " ++ show x
