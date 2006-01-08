
module Constraint.ReqMonad(ReqMonad, runReqMonad, find, finds) where

import General.StateMonad
import Constraint.Constraint
import Constraint.Req
import Pred.Type


type ReqMonad a = Transform a

runReqMonad :: [Req] -> ReqMonad a -> a
runReqMonad i v = snd $ applyStateMonad v (MyState [])



find :: Req -> ReqMonad Reqs
find x = writeState (\s -> let see = seen s
                           in
                                if x `elem` see then
                                    (s, predTrue)
                                else
                                    (MyState (x:see), predLit x)
                    )


finds :: Reqs -> ReqMonad Reqs
finds x = mapPredLitM find x


{-


runCounter :: Int -> Counter b -> b
runCounter i v = snd $ applyStateMonad v (MyState [i..])


getCount = getFreeInt


getCounts :: Int -> Counter [Int]
getCounts 0 = return []
getCounts n = do x <- getCount
                 y <- getCounts (n-1)
                 return (x:y)
                 
-}


data MyState = MyState { seen :: [Req] }

type Transform a = StateMonad MyState a

{-
getFreeInt :: Transform Int
getFreeInt = writeState (\s -> let (i:is) = freeInts s
                                   s' = s { freeInts = is }
                               in (s', i))

doStuff :: String -> [(Char,Int)]
doStuff s = snd $ applyStateMonad (transform s) initState
  where
  initState = MyState [0..]

transform :: String -> Transform [(Char,Int)]
transform s = mapM transOne s

transOne :: Char -> Transform (Char, Int)
transOne c = do i <- getFreeInt
                return (c, i) 
-}
