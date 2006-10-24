
module Hill.Dictionary(cmdsDictionary) where

import Hill.Type
import Data.List
import Data.Char
import General.TextUtil
import General.General



cmdsDictionary = [hillCmdPure "remove-dict" (const removeDictionary)]



data Dict = Dict {
                dictImp :: String,
                dictClass :: String,
                dictClassMod :: String,
                dictType :: String,
                dictTypeMod :: String
            }




-- a





removeDictionary :: Hill -> Hill
removeDictionary hill = hill{datas = newdatas ++ datas hill, funcs = newfuncs ++ concatMap usedatas (funcs hill)}
    where
        dicts = getDictionaries hill
        classes = groupSetExtract dictClass dicts
        
        newdatas = map f classes
            where
                f xs@(x:_) = Data ("Dict" ++ dictClass x) (map g xs) []
                g x = Ctor (dictClass x ++ dictType x) [] []

        newfuncs = concatMap f classes
            where
                f xs@(x:_) = zipWith (g xs) [0..] $ getMembers $ getFunc hill $ dictImp x
                
                g xs pos member = Func name [0] (Case (Var 0) alts)
                    where
                        name = dictClassMod (head xs) ++ "." ++ (reverse $ takeWhile (/= '.') $ reverse member)
                        alts = [AltCtr (dictClass x ++ dictType x) (Fun $ mems !! pos) | x <- xs,
                                        let mems = getMembers $ getFunc hill $ dictImp x]
            
            
                getMembers (Func _ _ (Apply _ xs)) = map fromFun xs

        
        usedatas x | funcName x `elem` map funcName newfuncs = []
                   | otherwise = [mapOverHill f x]
            where
                f (Fun y) = case [d | d <- dicts, dictImp d == y] of
                                [] -> Fun y
                                [x] -> Ctr (dictClass x ++ dictType x)
                f x = x




getDictionaries :: Hill -> [Dict]
getDictionaries hill = concatMap f (funcs hill)
    where
        f (Func name args body) | isDictionary body =
                if length nams == 5 then [Dict name (nams !! 2) (nams !! 1) (nams !! 4) (nams !! 3)]
                else error $ show ("Hill.Dictionary.getDictionaries",name)
            where
                nams = splitList "." name
        f _ = []
        
        isDictionary (Apply (Ctr tup) xs) = isTuple tup && all isFun xs
        isDictionary _ = False


        isTuple x | "Prelude." `isPrefixOf` x = isTuple $ drop 8 x
                  | otherwise = "(" `isPrefixOf` x && ")" `isSuffixOf` x && all (== ',') (init $ tail x)
