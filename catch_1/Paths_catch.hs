
module Paths_catch(version,getDataDir) where

import Data.Version

version = Version {versionBranch = [0,1], versionTags = []}

getDataDir :: IO FilePath
getDataDir = return ""
