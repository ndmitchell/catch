
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
removeDictionary hill = hill{datas = newdatas ++ datas hill, funcs = map usedatas (newfuncs ++ filter elimold (funcs hill))}
    where
        dicts = getDictionaries hill
        classes = groupSetExtract dictClass dicts
        
        newdatas = map f classes
            where
                f xs@(x:_) = Data ("Dict" ++ dictClass x) (map g xs) []
                g x = Ctor (dictClass x ++ dictType x) [] []

        newpairs = concatMap f dicts
            where
                f x = zipWith (\n a -> ((cls,n),a)) [0..] members
                    where
                        members = getMembers $ getFunc hill $ dictImp x
                        cls = dictClass x ++ dictType x
                        
        getMembers (Func _ _ (Apply _ xs)) = map fromFun xs
        
        newfuncs = concatMap (f . head) classes
            where
                f x = zipWith (g x) [0..] $ getMembers $ getFunc hill $ dictImp x
                
                g x pos member = Func name [0] (Case (Var 0) alts)
                    where
                        memname = reverse $ takeWhile (/= '.') $ reverse member
                        name = if isUpper (head memname)
                               then concat $ intersperse "." (init (splitList "." (dictImp x)) ++ [splitList "." member !! 2])
                               else dictClassMod x ++ "." ++ memname
                        alts = [AltCtr a (Fun c) | ((a,b),c) <- newpairs, b == pos, a == dictClass x ++ dictType x]

        
        elimold x = not $ funcName x `elem` map funcName newfuncs

        usedatas x = mapOverHill f x
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
                else if length nams == 7 then [Dict name (nams !! 3) (nams !! 2) (nams !! 6) ((nams !! 4) ++ "." ++ (nams !! 5))]
                else error $ show ("Hill.Dictionary.getDictionaries",name)
            where
                nams = splitList "." name
        f _ = []
        
        isDictionary (Apply (Ctr tup) xs) = isTuple tup && all isFun xs
        isDictionary _ = False


        isTuple x | "Prelude." `isPrefixOf` x = isTuple $ drop 8 x
                  | otherwise = "(" `isPrefixOf` x && ")" `isSuffixOf` x && all (== ',') (init $ tail x)
