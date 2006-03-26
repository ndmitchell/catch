{-|
    The command line in Catch is implemented as a function pipeline
    
    It is basically a standalone type checker
-}
module CmdLine(exec) where

import Hite
import Core
import Convert.CoreHite

import List
import Maybe
import Char
import General.TextUtil
import Directory
import System
import Monad

import Checker.CaseCheck


import General.Commands


data Arg = File String
         | Special SpecialArg
         | Terminal (Hite -> IO ())
         | Single String (Command Hite)
         | Unknown String

data SpecialArg = Verbose | Help | Version
                  deriving Eq

data Composite = Composite String String [String]
               

isSpecial (Special{}) = True; isSpecial _ = False
isFile (File{}) = True; isFile _ = False
isTerminal (Terminal{}) = True; isTerminal _ = False

-- Special functions
specials = [("verbose",Verbose),("help",Help),("version",Version)]

-- Terminal functions
terminals = [("safe-patterns",showCaseCheck),
             ("unsafe-patterns",term "unsafe-patterns"),
             ("statistics",term "statistics")]

showCaseCheck h = do outputHite "safe-patterns" h
                     caseCheck h

term :: String -> Hite -> IO ()
term s h = putStrLn $ "Terminal: " ++ s


verboseOut :: String -> Command Hite
verboseOut msg = Command (const f) undefined undefined
    where
        f :: Hite -> IO Hite
        f h = do outputHite msg h
                 return h

outputHite :: String -> Hite -> IO ()
outputHite msg h = do putStrLn $ "== " ++ msg
                      print h
                      putStrLn "================================================"


composite :: IO [Composite]
composite = do src <- readFile "catch.txt"
               return $ tail $ f (Composite "" "" []) $ filter (not.null) $ dropWhile null $ lines src
    where
        f acc [] = [acc]
    
        f acc@(Composite name desc cmds) ((x1:xs):ss)
            | isSpace x1 =
                f (Composite name desc (cmds ++ ['-':dropWhile isSpace xs])) ss
        
            | otherwise =
                acc : f (newComp (x1:xs)) ss
        
        newComp xs | null b = Composite xs "" []
                   | otherwise = Composite a (dropWhile isSpace b) []
            where (a,b) = break isSpace xs
        

exec :: [String] -> IO ()
exec [] = putStrLn "Error, no arguments specified, try -help for options"

exec args = do comp <- composite
               let (spec,rarg) = partition isSpecial (parseArgs comp args)
                   specs = [x | Special x <- spec]
                   
               let unknown = [x | Unknown x <- rarg]
               when (not (null unknown)) $
                    do putStrLn $ "Unknown arguments: " ++ concat (intersperse ", " unknown)
                       exitWith (ExitFailure 1)

               when (Help `elem` specs) $
                    do putStr $ unlines $ helpMsg comp
                       exitWith ExitSuccess
                       
               when (Version `elem` specs) $
                    do putStr $ unlines $ versionMsg
                       exitWith ExitSuccess
               
               let (files,farg) = partition isFile rarg
               when (length files /= 1) $
                    do putStrLn $ "Expected one file, found " ++ show (length files)
                       exitWith (ExitFailure 1)

               let vargs = if Verbose `elem` specs
                    then addVerbose farg
                    else farg
               
               when (not (null vargs) && any isTerminal (init vargs)) $
                    do putStrLn "A terminal action is used not as the last item"
                       exitWith (ExitFailure 1)
               
               let oargs = if null vargs || not (isTerminal (last vargs))
                           then vargs ++ [Terminal (outputHite "final")]
                           else vargs
               
               let [File file] = files
               runArgs file oargs
    where
        addVerbose [] = []
        addVerbose [x] = [x]
        addVerbose (s@(Single _ (Command _ name _)) : xs) = s : Single "" (verboseOut ("verbose, " ++ name)) : addVerbose xs
        addVerbose (x:xs) = x : addVerbose xs


parseArgs :: [Composite] -> [String] -> [Arg]
parseArgs comps args = concatMap (parseArg comps) args


parseArg :: [Composite] -> String -> [Arg]
parseArg comp ('-':xs)
    | isJust aSpecial  = [Special $ fromJust aSpecial]
    | isJust aTerminal = [Terminal $ fromJust aTerminal]
    | isJust aCmd      = [Single aParam $ fromJust aCmd]
    | otherwise        = applyComposite comp xs

    where
        aSpecial = lookup xs specials
        aTerminal = lookup xs terminals
        aCmd = listToMaybe [c | c@(Command _ name _) <- cmds, name == arg]
        
        (arg,param) = break (== '=') xs
        aParam = if null param then "" else tail param

parseArg comp s = [File s]


applyComposite :: [Composite] -> String -> [Arg]
applyComposite comp cmd = if null comps
                          then [Unknown cmd]
                          else parseArgs comp (head comps)
    where
        comps = [cmds | Composite name _ cmds <- comp, name == cmd]


versionMsg = ["Catch - Case and Termination Checker for Haskell"
             ,"(C) Neil Mitchell 2004-2006, http://www.cs.york.ac.uk/~ndm/"
             ]


helpMsg :: [Composite] -> [String]
helpMsg comp = versionMsg ++
               f [("Main Commands",imptCmds),
                  ("Meta Commands",g specials),
                  ("Terminal Commands",g terminals),
                  ("Transformation Commands",transCmds)]
    where
        imptCmds = [(name,tail desc) | Composite name desc _ <- comp, "*" `isPrefixOf` desc]
        transCmds = [(name,desc) | Command _ name desc <- cmds]
        g lst = map (\(a,b) -> (a,"")) lst
        
        f xs = concatMap (showGroup longest) items
            where
                longest = maximum [length arg | (grp,args) <- xs, (arg,desc) <- args]
                items = [(grp,map getDesc args) | (grp, args) <- xs]
                
        showGroup longest (grp, items) = "" : grp : map (showItem longest) items
        
        showItem longest (name,desc) = "* " ++ name ++ padding ++ " " ++ descs
            where
                padding = replicate (longest - length name) ' '
                indent = 3 + longest
                descs = concat $ intersperse ('\n' : replicate indent ' ') $ splitWidth (70-indent) desc
        
        getDesc (name,"") = (name, if null cmp then "" else head cmp)
            where cmp = [desc | Composite n2 desc _ <- comp, n2 == name]
        getDesc x = x


splitWidth :: Int -> String -> [String]
splitWidth norig xs = f norig [] (words xs)
    where
        f n acc [] = [unwords $ reverse acc]
        f n [] (x:xs) = f (n - length x) [x] xs
        f n acc (x:xs) | n2 < 0 = unwords (reverse acc) : f norig [] (x:xs)
                       | otherwise = f n2 (x:acc) xs
            where n2 = n - length x
        

runArgs :: String -> [Arg] -> IO ()
runArgs file acts = do hite <- readFileHite file
                       f hite acts
    where
        f hite (Single param (Command act _ _) : acts) =
            do h2 <- act param hite
               f h2 acts

        f hite [Terminal act] = act hite


readFileHiteRaw :: String -> IO Core
readFileHiteRaw x = do src <- readAny possFiles
                       return $ readCore src
    where
        possFiles = [x, "Example/" ++ x, "Example/" ++ x ++ ".hs"]
        
        readAny [] = error $ "File not found, " ++ x
        readAny (x:xs) = do b <- doesFileExist x
                            if b
                                then readCoreFile x
                                else readAny xs
        
        readCoreFile :: FilePath -> IO String
        readCoreFile hs = do b <- doesFileExist cr
                             if not b then createCore hs cr
                              else do
                                      hsMod <- getModificationTime hs
                                      crMod <- getModificationTime cr
                                      if hsMod > crMod then createCore hs cr else checkCore hs cr
            where
                cr = hs ++ ".core"

        createCore hs cr = do system $ "yhc " ++ hs ++ " -corep 2> " ++ cr
                              checkCore hs cr
        
        checkCore hs cr = do b <- doesFileExist cr
                             if not b then
                                error "Core file not created, do you have Yhc?"
                                else readFile cr


readFileHite :: String -> IO Hite
readFileHite x = do preamble <- readFileHiteRaw "Preamble/Preamble.hs"
                    self <- readFileHiteRaw x
                    return $ coreHite (mergeCore preamble self)

{-



                 


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
        inp2 <- case inp of
                   Just a -> return $ DatString a
                   Nothing -> do x <- return files
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

-}
