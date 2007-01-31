
module Req(module Req, module Val) where

import Val
import Yhc.Core
import Data.Proposition


-- DATA DEFINITIONS

type Scopes = [Scope]
data Scope = Scope CoreFuncName Vals

type Reqs = PropSimple Req

data Req = Req CoreExpr Vals
         | Demonic
         | Angelic
         deriving (Ord, Eq)


instance Show Scope where
    show (Scope name reqs) = "(\\forall " ++ name ++ ", " ++ show reqs ++ ")"

instance Show Req where
    show (Req expr x) = showCoreExprGroup expr ++ show x
    show Demonic = "?Demonic"
    show Angelic = "?Angelic"


instance PropLit Req where
    -- do not define anything, its a pretty bad PropLit!
