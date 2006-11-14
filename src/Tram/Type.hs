
module Tram.Type(module Tram.Path, module Tram.Type, module Tram.Req) where

import Tram.Path
import Tram.Req

import Hill.All
import Data.List


defaultAlts :: Hill -> [Alt] -> [CtorName]
defaultAlts hill alts = (ctorOthers $ getCtor hill $ altCtr $ head alts) \\ [x | AltCtr x _ <- alts]