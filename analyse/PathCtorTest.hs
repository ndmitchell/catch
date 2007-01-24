
module Main where

import PathCtor
import PathCtorEq
import SmallCheck
import Data.Proposition
import Data.Maybe
import Debug.Trace
import System.Environment


-- COMMAND LINE HANDLER

go x = smallCheck 10 x

actions = [("correct_atom"  , go correct_atom  )
          ,("confluent_atom", go confluent_atom)
          ,("correct_or"    , go correct_or    )
          ,("confluent_or"  , go confluent_or  )
          ]


main :: IO ()
main = do args <- getArgs
          let res = lookup (head args) actions
          case args of
              [x] | isJust res -> fromJust res
              _ -> putStrLn $ "Expected one of: " ++ unwords (map fst actions) ++ "\n" ++
                              "Found: " ++ show args


-- UTILITY FUNCTIONS

type PathCtors = PropSimple PathCtor

simplifyEither :: PathCtor -> BoolPathCtor
simplifyEither (PathCtor core path ctors) = newPathCtor core path ctors

simplify :: PathCtor -> PathCtors
simplify = fromEither . simplifyEither

fromBool :: Bool -> PathCtors
fromBool x = if x then propTrue else propFalse

toBool :: PathCtors -> Maybe Bool
toBool x | propIsTrue  x = Just True
         | propIsFalse x = Just False
         | otherwise = Nothing

fromReduce :: Reduce PathCtor -> PathCtors
fromReduce (Value x) = propLit x
fromReduce (Literal x) = fromBool x

fromEither :: BoolPathCtor -> PathCtors
fromEither (Left x) = fromBool x
fromEither (Right x) = propLit x



normalPath (Left x) = if x then truePathCtor else falsePathCtor
normalPath (Right x) = x

reducePath (Value x) = x
reducePath (Literal x) = if x then truePathCtor else falsePathCtor


rePathCtor (PathCtor core path ctors) = newPathCtor core path ctors


isRight (Right{}) = True; isRight _ = False


-- PROPERTIES

correct_atom :: PathCtor -> Property
correct_atom orig = new /= orig2 ==>
        if equalPathCtorProp orig2 new then True
        else error $ "correct_atom failed with " ++ show orig ++ ", which gets simplified to " ++ show new
    where
        orig2 = propLit orig
        new = simplify orig


confluent_atom :: PathCtor -> PathCtor -> Property
confluent_atom a b = equalPathCtor a b ==> simplify a == simplify b



correct_or :: PathCtor -> PathCtor -> Property
correct_or a b = isRight a2 && isRight b2 && res /= None ==>
        if equalPathCtorProp lhs rhs then True
        else error $ "correct_or failed with " ++ show lhs ++ ", which gets simplified to " ++ show rhs
    where
        (a2, b2) = (simplifyEither a, simplifyEither b)
        (Right a3, Right b3) = (a2, b2)

        lhs = propOr (propLit a3) (propLit b3)
        rhs = fromReduce res
        res = a3 ?\/ b3



confluent_or :: PathCtor -> PathCtor -> PathCtor -> Property
confluent_or a b c = isRight a2 && isRight b2 && equalPathCtorProp lhs (propLit c) ==>
                     if ans then True else trace (show (a2,b2,rePathCtor c)) True
    where
        ans = res /= None && normalPath (rePathCtor c) == reducePath res
        (a2,b2) = (rePathCtor a, rePathCtor b)
        (Right a3,Right b3) = (a2,b2)
    
        lhs = propOr (propLit a3) (propLit b3) :: PropSimple PathCtor
        
        res = a3 ?\/ b3

