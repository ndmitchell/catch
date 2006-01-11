{-|
    The command line in Catch is implemented as a function pipeline
    
    It is basically a standalone type checker
-}
module CmdLine(CmdOpt(..), CmdDat(..), CmdLine(..), exec) where

import Hite
import Core
import Convert.CoreHite

import List
import Maybe
import Char
import General.TextUtil
import Directory


import CmdLineData
import Actions


data Verbosity = Normal | Verbose | Quiet
                 deriving Eq


-- a DatString can automatically be converted to a DatHite

fullCmdLine = specials ++ test_cmdLine ++ Actions.cmdLine


specials = [f 0 "verbose" "output lots of information",
            f 1 "help" "show this help screen"
           ]
    where
        f n a b = CmdLine a (OptSpecial n) (OptSpecial n) (const return) b



execPipe :: [String] -> String -> IO CmdDat
execPipe args inp = execEither (Just inp) args


exec :: [String] -> IO ()
exec args = do execEither Nothing args
               return ()


execEither :: Maybe String -> [String] -> IO CmdDat
execEither inp args = 
    do
        allFiles <- collectFiles files
        inp2 <- case inp of
                   Just a -> return $ DatString a
                   Nothing -> do x <- collectFiles files
                                 return $ DatInputs x
        
        if null normals || (null files && isNothing inp) || showHelp
            then do putStrLn helpMsg
                    return (DatAction helpMsg)
         else if typeCheck (map fst normals)
            then runCommands verb normals inp2
         else error "internal error, typeCheck should not return false"
    where
        verb = if isJust inp then Quiet else if verbose then Verbose else Normal
        
        verbose  = 0 `elem` specialCodes
        showHelp = 1 `elem` specialCodes
    
        coms = map (getCmd . tail) flags
        
        
        specialCodes = map (\(CmdLine _ (OptSpecial n) _ _ _, _) -> n) spec
        (spec, normals) = partition (isSpecial . fst) coms
        (flags, files) = partition ("-" `isPrefixOf`) args
        
        getCmd name = case [c | c@(CmdLine n _ _ _ _) <- fullCmdLine, n == nam2] of
                           (x:_) -> (x, if null opt then [] else tail opt)
                           [] -> error $
                                "Unknown argument: " ++ name ++ ", try -help for valid arguments"
            where
                nam2 = map toLower nam
                (nam, opt) = break (== '=') name

isSpecial (CmdLine _ (OptSpecial _) _ _ _) = True
isSpecial _ = False


collectFiles :: [FilePath] -> IO [FilePath]
collectFiles x = concatMapM f x
    where
        f x = do isDir <- doesDirectoryExist x
                 isFile <- doesFileExist x
                 if isDir then
                     do xs <- getDirectoryContents x
                        concatMapM (f . (++) (x ++ "/")) $ filter isReal xs
                  else if isFile then
                     return [x]
                  else
                     error $ "ERROR: Not a file or folder, " ++ x
            where
                isReal "." = False
                isReal ".." = False
                isReal "CVS" = False
                isReal _ = True


concatMapM f xs = do x <- mapM f xs
                     return $ concat x


helpMsg :: String
helpMsg = unlines $ 
        [
            "Catch - Case And Termination Checker for Haskell",
            "(C) Neil Mitchell 2004-2006",
            "http://www.cs.york.ac.uk/~ndm/projects/catch.php",
            ""
        ] ++ map f fullCmdLine
    where
        longestName = maximum $ map (length . cmdLineName) fullCmdLine
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
            else error $ "Incompatible types: " ++
                         n1 ++ "(" ++ show input  ++ ")" ++ " -> " ++
                         n2 ++ "(" ++ show output ++ ")"
        f _ = True


compatType :: CmdOpt -> CmdOpt -> Maybe (CmdDat -> IO CmdDat)
compatType a b | a == b = Just return
compatType OptString OptHite = Just (\(DatString x) -> return $ DatHite (read x))
compatType OptString OptCore = Just (\(DatString x) -> return $ DatCore (readCore x))
compatType OptCore   OptHite = Just (\(DatCore x  ) -> return $ DatHite (coreHite x))

-- is never used, always handled by the runCommands function
compatType OptInputs b = compatType OptString b

compatType _ _ = Nothing


runCommands :: Verbosity -> [(CmdLine, String)] -> CmdDat -> IO CmdDat
runCommands verbose actions inp =
    do
        s <- begin
        f actions s
    where
        f [(a,v)] s = g (verbose /= Quiet) a s v

        f ((a,v):as) s = do s2 <- g (verbose == Verbose) a s v
                            f as s2
        
        g disp (CmdLine name input output act _) s v = 
                do  out $ replicate 40 '-' ++ "\n-- Result after " ++ name ++ "\n"
                    s2 <- conv s
                    s3 <- act v s2
                    out $ show s3
                    return s3
            where
                conv = fromJust $ compatType (datToOpt s) input
                out x = if disp then putStrLn x else return ()
    
        begin :: IO CmdDat
        begin = case (actions, inp) of
                    (((CmdLine _ OptInputs _ _ _,_):_), DatInputs x) -> return inp
                    (_, DatString x) -> return inp
                    (_, DatInputs [x]) -> do y <- readFile x
                                             return $ DatString y



---------------------------------------------------------------------
-- module Test where
-- because of lack of mututally recursive modules, these modules
-- need to be in the same source file

test_cmdLine = [CmdLine "test" OptInputs OptAction (const tester) "Performs some regression tests"]


tester :: CmdDat -> IO CmdDat
tester (DatInputs xs) = do rep <- mapM testFile xs
                           let repn = zip3 [1..] rep xs
                               progress = unlines $ map f repn
                               reports = concatMap g repn
                               summary = "Passed " ++ show (lxs - length (catMaybes rep)) ++ "/" ++ show lxs
                           return $ DatAction $ progress ++ reports ++ "\n" ++ summary
    where
        llxs = length (show lxs)
        lxs = length xs
        
        pad n = show n ++ replicate (llxs - length (show n)) ' '
        
        f :: (Int, Maybe String, FilePath) -> String
        f (n, res, file) = 
            "Test " ++ pad n ++ "/" ++ show lxs ++ " - " ++ 
            if isJust res then "FAIL" else "pass" ++
            " (" ++ file ++ ")"
        
        g :: (Int, Maybe String, FilePath) -> String
        g (n, Nothing, _) = ""
        g (n, Just x , _) = "Detailed report for " ++ show n ++ "\n"


-- if you fail, return Just Result
-- otherwise return Nothing for success
testFile :: FilePath -> IO (Maybe String)
testFile file = do x <- readFile file
                   let xs = lines x
                       params = splitList " " (head xs)
                       brk = xs !! 1
                       (input, _:output) = break (== brk) (drop 2 xs)
                       out = unlines output
                   res <- execPipe params (unlines input)
                   return Nothing
                   

eqHite :: String -> String -> Bool
eqHite a b = (read a :: Hite) == (read b :: Hite)

