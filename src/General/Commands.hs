
module General.Commands where


data Command a = Command (String -> a -> IO a) String String

