
module Train.Template(Template, templateInit, templateGet) where

import Train.Type
import System.IO
import Data.Predicate


data Template = Template


templateInit :: ZHite -> Handle -> IO Template
templateInit _ _ = return Template


-- first element of Req must be a ZCall
templateGet :: Template -> Req -> IO Reqs
templateGet _ _ = return predFalse
