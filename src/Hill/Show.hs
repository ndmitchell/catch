
module Hill.Show where

import Hill.Type
import List
import General.General
import Text.PrettyPrint.HughesPJ


instance Show Hill where
    show (Hill datas funcs) = unlines (map show datas ++ map (('\n':) . show) funcs)
    

instance Show Func where
    show x = render $ docFunc x

instance Show Expr where
    show x = render $ docExpr x



docFunc :: Func -> Doc
docFunc (Func name args expr) = text initial <>> docExpr expr
    where initial = name ++ concatMap ((' ':) . ('@':) . show) args ++ " ="


docExpr :: Expr -> Doc
docExpr x = f 0 x
    where
        -- number mean:
        -- 0 is do deep lets
        -- 1 is do identation bracketing
        -- 2 is do real bracketing
    
        f _ (Var name) = text $ '@': show name
        f _ (Star) = char '?'
        f _ (Fun x) = text x
        f _ (Ctr x) = text x
        f _ (Const x) = text $ show x
        
        f _ (Sel x y) = f 2 x <> text ('.':y)

        f _ (Call x []) = text x
        f i (Call x xs) = (if i >= 2 then parens else id) $
                          call (text x) (map (f 2) xs)
        
        f i (Prim x xs) = f i (Call (x++"#") xs)
        f i (Make x xs) = f i (Call x xs)
        f i (Error x) = f i (Prim "error" [x])
        
        f _ (Apply x xs) = braces $ call (f 2 x) (map (f 2) xs)
        
        f i (Let binds x) = text "let" <+> vcat (map g binds) $$ text "in" <+> f i x
            where
                g (lhs,rhs) = text ("@" ++ show lhs ++ " =") <+> f i rhs

        f i (Case on alts) =
                (if i >= 2 then parens else id) $
                text "case" <+> f i on <+> text "of" $$ inner (vcat $ map g alts)
            where
                g x = (h x <+> text "->") <>> f i (altExpr x)
            
                h (Default x) = text "_"
                h (AltCtr x _) = text x
                h (AltConst x _) = text $ show x

        f i x = error "Hill.Show, unhandled"
        
        -- implement a call
        call x xs = sep $ x : map (nest 2) xs
    
    
inner = nest 4


a <>> b = sep [a, inner b]


instance Show Const where
    show x = case x of
        AInt y -> show y
        AInteger y -> '0' : show y
        AFloat y -> show y
        ADouble y -> show y
        AChar y -> show y
        AString y -> show y
