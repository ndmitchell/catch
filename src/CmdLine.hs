{-|
    The command line in Catch is implemented as a function pipeline
    
    It is basically a standalone type checker
-}
module CmdLine(CmdOpt(..), CmdDat(..), CmdLine(..), exec) where

import Hite.Type
import Hite.Read
import Hite.Show

import List
import Maybe
import Char


data CmdOpt = OptString | OptHite | OptAction | OptInputs | OptSpecial Int
              deriving Eq


instance Show CmdOpt where
    show OptString = "string"
    show OptHite = "Hite"
    show OptAction = "()"
    show OptInputs = "*"


data CmdDat = DatString String | DatHite Hite | DatAction String | DatInputs [String]

datToOpt (DatString _) = OptString
datToOpt (DatHite   _) = OptHite
datToOpt (DatAction _) = OptAction
datToOpt (DatInputs _) = OptInputs



data CmdLine = CmdLine String CmdOpt CmdOpt (String -> CmdDat -> IO CmdDat) String


instance Show CmdLine where
    show c = cmdLineName c


instance Show CmdDat where
    show (DatString x) = x
    show (DatHite x) = show x
    show (DatAction x) = x


cmdLineName (CmdLine a _ _ _ _) = a

-- a DatString can automatically be converted to a DatHite


specials = [f 0 "verbose" "output lots of information",
            f 1 "help" "show this help screen"
           ]
    where
        f n a b = CmdLine a (OptSpecial n) (OptSpecial n) (const return) b


exec :: [CmdLine] -> [String] -> IO ()
exec cmds args = 
            if null normals || null files || showHelp then putStrLn (helpMsg cmds2)
            else if typeCheck (map fst normals) then runCommands verbose normals files
            else error "internal error, typeCheck should not return false"
    where
        verbose  = 0 `elem` specialCodes
        showHelp = 1 `elem` specialCodes
    
        cmds2 = specials ++ cmds
        coms = map (getCmd . map toLower . tail) flags
        
        
        specialCodes = map (\(CmdLine _ (OptSpecial n) _ _ _, _) -> n) spec
        (spec, normals) = partition (isSpecial . fst) coms
        (flags, files) = partition ("-" `isPrefixOf`) args
        
        getCmd name = case [c | c@(CmdLine n _ _ _ _) <- cmds2, n == nam] of
                           (x:_) -> (x, if null opt then [] else tail opt)
                           [] -> error $
                                "Unknown argument: " ++ name ++ ", try -help for valid arguments"
            where
                (nam, opt) = break (== '=') name

isSpecial (CmdLine _ (OptSpecial _) _ _ _) = True
isSpecial _ = False



helpMsg :: [CmdLine] -> String
helpMsg xs = unlines $ 
        [
            "Catch - Case And Termination Checker for Haskell",
            "(C) Neil Mitchell 2004-2006",
            "http://www.cs.york.ac.uk/~ndm/projects/catch.php",
            ""
        ] ++ map f xs
    where
        longestName = maximum $ map (length . cmdLineName) xs
        f c@(CmdLine name input output _ desc) =
            "   -" ++ pad name ++ "  " ++ desc ++
            if isSpecial c then "" else " (" ++ show input ++ "->" ++ show output ++ ")"
        
        pad x = x ++ replicate (longestName - length x) ' '



typeCheck :: [CmdLine] -> Bool
typeCheck cmds = f (begin:cmds)
    where
        begin = CmdLine "<input>" OptAction OptInputs (const return) ""
    
        f (CmdLine n1 _ input _ _ : c@(CmdLine n2 output _ _ _) : rest) =
            if isJust (compatType input output) then f (c:rest)
            else error $ "Incompatible types: " ++ n1 ++ " -> " ++ n2
        f _ = True


compatType :: CmdOpt -> CmdOpt -> Maybe (CmdDat -> IO CmdDat)
compatType a b | a == b = Just return
compatType OptString OptHite = Just (\(DatString x) -> return $ DatHite (read x))

-- is never used, always handled by the runCommands function
compatType OptInputs b = compatType OptString b

compatType _ _ = Nothing


runCommands :: Bool -> [(CmdLine, String)] -> [FilePath] -> IO ()
runCommands verbose actions files =
    do
        s <- begin
        f actions (head s)
    where
        f [(a,v)] s = do g True a s v
                         return ()

        f ((a,v):as) s = do s2 <- g verbose a s v
                            f as s2
        
        g disp (CmdLine name input output act _) s v = 
                do  out $ replicate 40 '-' ++ "\n-- Result after " ++ name ++ "\n"
                    s2 <- conv s
                    s3 <- act v s2
                    out $ show s3
                    return s3
            where
                conv = fromJust $ compatType (datToOpt s) output
                out x = if disp then putStrLn x else return ()
                
    
    
        begin :: IO [CmdDat]
        begin = case actions of
                    ((CmdLine _ OptInputs _ _ _,_):_) -> return [DatInputs files]
                    _ -> do x <- mapM readFile files
                            return $ map DatString x
