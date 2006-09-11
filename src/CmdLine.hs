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
import CPUTime
import Time
import IO

import Make
import Make2

import Checker.CaseCheck
import Checker.Statistics
import Graph.CaseCheck
import Subst.CaseCheck
import Typey.CaseCheck
import Train.Driver
import Backend.Backend


import General.Commands
import General.General


data Arg = File String
         | Special SpecialArg
         | Terminal (FilePath -> Hite -> IO Bool)
         | Single String (Command Hite)
         | UnknownArg String

data SpecialArg = Verbose | Help | Version | Profile | Clean | Reclean
                  deriving Eq

data Composite = Composite String String [String]
               

isSpecial (Special{}) = True; isSpecial _ = False
isFile (File{}) = True; isFile _ = False
isTerminal (Terminal{}) = True; isTerminal _ = False

-- Special functions
specials = [("verbose",Verbose),("help",Help),("version",Version),
            ("profile",Profile),("clean",Clean),("reclean",Reclean)]

-- Terminal functions
terminals = [("safe-patterns",term "safe-patterns" caseCheck),
             ("unsafe-patterns",term "unsafe-patterns" undefined),
             ("graph-patterns",term "graph-patterns" graphCaseCheck),
             ("subst-patterns",term "subst-patterns" substCaseCheck),
             ("typey-patterns",term "typey-patterns" typeyCaseCheck),
             ("typeyho-patterns",term "typeyho-patterns" typeyHoCaseCheck),
             ("abstract-patterns", term "abstract-patterns" abstractCaseCheck),
             ("train-patterns", term "train-patterns" trainDriver),
             ("backend", term "backend" backend)
             {- ,
             ("statistics", \a b -> statistics) -} ]

term :: String -> (String -> Handle -> Hite -> IO Bool) -> FilePath -> Hite -> IO Bool
term s f out hite = do ensureDirectory "Logs"
                       handle <- openFile ("Logs/" ++ out ++ ".log") WriteMode
                       putStrLn "Generating reduced Haskell"
                       hPutStrLn handle $ "== " ++ s
                       hPutStrLn handle $ output hite
                       hPutStrLn handle $ "================================================"
                       putStrLn $ "Begining analysis: " ++ s
                       res <- f out handle hite
                       hClose handle
                       return res


verboseOut :: String -> Command Hite
verboseOut msg = Command (const f) undefined undefined
    where
        f :: Hite -> IO Hite
        f h = do outputHite msg h
                 return h

outputHite :: String -> Hite -> IO ()
outputHite msg h = do putStrLn $ "== " ++ msg
                      putStrLn $ output h
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
                   
               let unknown = [x | UnknownArg x <- rarg]
               when (not (null unknown)) $
                    do putStrLn $ "UnknownArg arguments: " ++ concat (intersperse ", " unknown)
                       exitWith (ExitFailure 1)

               when (Help `elem` specs) $
                    do putStr $ unlines $ helpMsg comp
                       exitWith ExitSuccess
                       
               when (Version `elem` specs) $
                    do putStr $ unlines $ versionMsg
                       exitWith ExitSuccess
                       
               let (files2,farg) = partition isFile rarg
               files <- expandWildcard (map (\(File file) -> file) files2)

               args <- return farg
               args <- return $ if Verbose `elem` specs
                    then addVerbose args
                    else args
               
               args <- return $ if Profile `elem` specs
                    then addProfile args
                    else args
               
               when (not (null args) && any isTerminal (init args)) $
                    do putStrLn "A terminal action is used not as the last item"
                       exitWith (ExitFailure 1)
               
               args <- return $ if null args || not (isTerminal (last args))
                           then args ++ [Terminal (\a b -> outputHite "final" b >> return True)]
                           else args

               when (null files) $ do
                    putStr "No files specified or found, nothing to do"
                    exitWith ExitSuccess

               runAll args files
    where
        runAll args files = do res <- mapM f files
                               let (success, failed) = partition fst $ zip res files
                               when (length files > 1) $ do
                                   putStrLn "==== RESULTS ===="
                                   putStrLn $ "Success: " ++ pretty success
                                   putStrLn $ "Failure: " ++ pretty failed
            where
                pretty [] = "<none>"
                pretty xs = concat $ intersperse ", " $ map snd xs
            
                f file = do putStrLn $ "==== RUNNING: " ++ file ++ " ===="
                            res <- runArgs file args
                            putStrLn "\n"
                            return res
    
        expandWildcard ["!"] = do src <- readFile "Example/SafePatterns.txt"
                                  return $ map (takeWhile (/= ' ')) $ lines src
        expandWildcard x = return x
    
    
        addVerbose [] = []
        addVerbose [x] = [x]
        addVerbose (s@(Single _ (Command _ name _)) : xs) = s : Single "" (verboseOut ("verbose, " ++ name)) : addVerbose xs
        addVerbose (x:xs) = x : addVerbose xs
        
        addProfile xs = (concatMap f $ filter (not.isTerminal) xs) ++ [Terminal profEnd]
            where
                f sing@(Single _ (Command _ name _)) = [Single "" (Command (prof name) "" ""), sing]
                
                prof s arg hite = do c <- getCPUTime
                                     print $ if length (output hite) > 0 then c else 0
                                     putStrLn s
                                     return hite
                                     
                profEnd hs h = do prof "end" "" h 
                                  return True
                                        

removeExist :: FilePath -> IO ()
removeExist file = do b <- doesFileExist file
                      when b $ removeFile file


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
                          then [UnknownArg cmd]
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
        

runArgs :: String -> [Arg] -> IO Bool
runArgs file acts = do -- hs <- pickFile file
                       hite <- make2 file
                       f file hite acts
    where
        f hs hite (Single param (Command act _ _) : acts) =
            do h2 <- act param hite
               f hs h2 acts

        f hs hite [Terminal act] = act hs hite

{-

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
-}

pickFile :: FilePath -> IO FilePath
pickFile x = f [x, "Example/" ++ x, "Example/" ++ x ++ ".hs"]
    where
        f [] = error $ "File not found, " ++ x
        f (x:xs) = do b <- doesFileExist x
                      if b then return x else f xs


readFileHite :: String -> IO Hite
readFileHite x = do hs <- pickFile x
                    hsd <- getModificationTime hs
                    let hite = hs ++ ".hite"
                    
                    mhite <- isModified hsd hite
                    if not mhite then do
                        putStrLn $ "Reading hite from cache: " ++ hs
                        readCacheHite hite
                     else do
                        hp <- readFileCore True "Preamble/Preamble.hs"
                        hs <- readFileCore False hs
                        let h2 = reachable "main" $ shortName $ coreHite $ mergeCore hp hs
                        writeCacheHite h2 hite
                        return h2


readFileCore :: Bool -> FilePath -> IO Core
readFileCore prel hs = do hsd <- getModificationTime hs
                          let core = hs ++ ".core"

                          mcore <- isModified hsd core
                          if mcore
                             then createCore hs core
                             else putStrLn $ "Reading core from cache: " ++ hs

                          src <- readFile core
                          return $ readCore src
    where
        createCore hs cr = do putStrLn $ "Compiling core file for: " ++ hs
                              let cr2 = cr ++ ['2'|prel]
                              system $ "yhc " ++ hs ++ " -corep 2> " ++ cr2
                              b <- doesFileExist cr2
                              when (not b) $ error "Core file not created, do you have Yhc?"
                              when prel $ do
                                    src <- readFile (cr ++ "2")
                                    writeFile cr (stripPreamble src)
                                    


stripPreamble src = strip src                             
    where
        tupStart = ",CoreFunc (CoreApp (CoreVar \"Preamble.tup"
        tupEnd = ")]))"

        strip x | tupStart `isPrefixOf` x = strip (dropTup x)
        strip (x:xs) = x : strip xs
        strip [] = []


        dropTup x | tupEnd `isPrefixOf` x = drop (length tupEnd) x
        dropTup (x:xs) = dropTup xs


isModified :: ClockTime -> FilePath -> IO Bool
isModified ct file = do b <- doesFileExist file
                        if b then
                            do ct2 <- getModificationTime file
                               return $ ct > ct2
                         else
                            return True

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
                                "UnknownArg argument: " ++ name ++ ", try -help for valid arguments"
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
