
module Subst.Show where

import Subst.Type

import Data.List
import qualified Data.Map as Map
import qualified Data.IntMap as IntMap


instance Show Env where
    show (Env subst bind) = unlines $
            ["ENVIRONMENT:"] ++
            map showSubst (IntMap.toList subst) ++
            map showBind (Map.toList bind)
        where
            showSubst (n,e) = "  " ++ show n ++ " = " ++ show e
            showBind ((f,a),e) = "  " ++ show f ++ concatMap (\x -> " ("++show x++")") a ++ " = " ++ show e
            

instance Show SExp where
    show SFree = "?"
    show SBot = "_|_"
    show (SVar n) = show n
    show (SCtor name args) = "(" ++ name ++ concatMap (\x -> ' ':show x) args ++ ")"
    show (SFunc name args) = show (SCtor name args)
    show (SChoice xs) = "{" ++ concat (intersperse " " $ map show xs) ++ "}"
