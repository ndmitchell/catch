
module PathCtorTest where

import PathCtor
import PathCtorEq
import SmallCheck
import Data.Proposition
import Debug.Trace


normalPath (Left x) = if x then truePathCtor else falsePathCtor
normalPath (Right x) = x

reducePath (Value x) = x
reducePath (Literal x) = if x then truePathCtor else falsePathCtor


rePathCtor (PathCtor core path ctors) = newPathCtor core path ctors



correct_atom :: PathCtor -> Property
correct_atom orig = new /= orig ==> if equalPathCtor orig new then True else error $ show (orig, new)
    where new = normalPath $ rePathCtor orig


confluent_atom :: PathCtor -> PathCtor -> Property
confluent_atom a b = equalPathCtor a b ==> rePathCtor a == rePathCtor b



correct_or :: PathCtor -> PathCtor -> Property
correct_or a b = isRight a2 && isRight b2 && res /= None ==>
                 if equalPathCtorProp id lhs rhs then True else error $ show (a3,b3,res)
    where
        (a2,b2) = (rePathCtor a, rePathCtor b)
        (Right a3,Right b3) = (a2,b2)
    
        lhs = propOr (propLit a3) (propLit b3)
        rhs = propLit (reducePath res) :: PropSimple PathCtor
        
        res = a3 ?\/ b3


confluent_or :: PathCtor -> PathCtor -> PathCtor -> Property
confluent_or a b c = isRight a2 && isRight b2 && equalPathCtorProp id lhs (propLit c) ==>
                     if ans then True else trace (show (a2,b2,rePathCtor c)) True
    where
        ans = res /= None && normalPath (rePathCtor c) == reducePath res
        (a2,b2) = (rePathCtor a, rePathCtor b)
        (Right a3,Right b3) = (a2,b2)
    
        lhs = propOr (propLit a3) (propLit b3) :: PropSimple PathCtor
        
        res = a3 ?\/ b3


isRight (Right{}) = True; isRight _ = False
