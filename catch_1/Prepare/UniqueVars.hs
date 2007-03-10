
module Prepare.UniqueVars(uniqueVars) where

import Yhc.Core(Core)
import Yhc.Core.FreeVar2


uniqueVars :: Core -> Core
uniqueVars = runFreeVars . uniqueBoundVarsCore
