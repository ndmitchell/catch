
module Hill.Cmds(cmdsHill) where


-- a command is:
-- (name :: String, function :: String -> Hill -> IO Hill)


import Hill.Reachable


cmdsHill = cmdsReachable
