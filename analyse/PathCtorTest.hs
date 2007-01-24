
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
          ,("correct_and"   , go correct_and   )
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



correct_pair :: String -> (PathCtor -> PathCtor -> Reduce PathCtor) -> (PathCtors -> PathCtors -> PathCtors)
             -> PathCtor -> PathCtor -> Property
correct_pair msg reducer combiner a b = isRight a2 && isRight b2 && res /= None ==>
        if equalPathCtorProp lhs rhs then True
        else error $ msg ++ " failed with " ++ show lhs ++ ", which gets simplified to " ++ show rhs
    where
        (a2, b2) = (simplifyEither a, simplifyEither b)
        (Right a3, Right b3) = (a2, b2)

        lhs = combiner (propLit a3) (propLit b3)
        rhs = fromReduce res
        res = reducer a3 b3


correct_or :: PathCtor -> PathCtor -> Property
correct_or = correct_pair "correct_or" (?\/) propOr

correct_and :: PathCtor -> PathCtor -> Property
correct_and = correct_pair "correct_and" (?/\) propAnd


confluent_or :: PathCtor -> PathCtor -> PathCtor -> Property
confluent_or a b c = isRight a2 && isRight b2 && equalPathCtorProp lhs c2 ==>
        if res /= None && c2 == rhs then True
        else trace ("confluent_or missed with " ++ show lhs ++ ", which equals " ++ show c2) True
    where
        c2 = simplify c
        (a2,b2) = (simplifyEither a, simplifyEither b)
        (Right a3,Right b3) = (a2,b2)
    
        lhs = propOr (propLit a3) (propLit b3)
        rhs = fromReduce res
        res = a3 ?\/ b3

