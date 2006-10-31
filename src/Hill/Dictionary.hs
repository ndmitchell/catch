
module Hill.Dictionary(cmdsDictionary) where

import Hill.Type
import Data.List
import Data.Char
import General.TextUtil
import General.General
import Debug.Trace


cmdsDictionary = [hillCmdPure "remove-dict" (const removeDictionary)]


data TypeValue = TypeValue {typeValueModu :: String, val :: String}
                 deriving (Show, Eq, Ord)

data TypeClass = TypeClass {typeClassModu :: String, cls :: String}
                 deriving (Show, Eq, Ord)

data Dictionary = Dictionary {dictName :: String, dictModu :: String
                             ,typVal :: TypeValue, typCls :: TypeClass}
                  deriving (Show, Eq, Ord)


removeDictionary :: Hill -> Hill
removeDictionary hill = hill{datas = newdatas ++ datas hill, funcs = newfuncs ++ filter elimold (funcs hill)}
    where
        dicts = getDictionaries hill

        typeFree (TypeValue modu val) =
            length $ concat [free | Data x _ free <- datas hill, x == modu ++ "." ++ val]

        
        newdatas = [Data "Type" ctrs []]
            where
                ctrs = map f $ snub $ map typVal dicts
            
                f t@(TypeValue modu val) = Ctor ("Type" ++ val)
                             ["type" ++ val ++ show i | i <- [1..fre]]
                             [] -- (replicate fre (TyCon "Type" []))
                    -- NOTE: If given a recursive type, this stops the specialiser from doing its job
                    where
                        fre = typeFree t
        
        newfuncs = newdicts ++ newaccess
        
        newdicts = map f dicts
            where
                f dict = Func (dictName dict) frees (Apply (Ctr $ "Type" ++ val (typVal dict)) (map Var frees))
                    where frees = [1..typeFree (typVal dict)]
        
        newaccess = concatMap f $ groupSetExtract typCls dicts
            where
                f xs@(x:_) = map (g (typCls x) xs) members
                    where
                        members = getMembers $ getFunc hill $ dictName x
                
                g typcls xs member = trace name $ Func name [0] (Case (Var 0) alts)
                    where
                        memname = (reverse . takeWhile (/= '.') . reverse) member
                        name = if isUpper (head memname) then name2 else typeClassModu typcls ++ "." ++ memname

                        dict = splitName member
                        name2 = dictModu dict ++ "." ++
                                typeClassModu typcls ++ "." ++ cls typcls ++ "." ++ 
                                typeClassModu (typCls dict) ++ "." ++ cls (typCls dict)

                        alts = [AltCtr
                                    ("Type" ++ tstr)
                                    (Apply (Fun $ dictName x ++ "." ++ memname) (map (\i -> Var 0 `Sel` ("type" ++ tstr ++ show i)) [1..typeFree (typVal x)]))
                                    | x <- xs, let tstr = val (typVal x)]
        
        getMembers (Func _ _ (Apply _ xs)) = map fromFun xs
        
        elimold x = not $ funcName x `elem` map funcName newfuncs



getDictionaries :: Hill -> [Dictionary]
getDictionaries hill = concatMap f (funcs hill)
    where
        f (Func name args body) | isDictionary args body = [splitName name]
        f _ = []
        
        isDictionary args (Apply (Ctr tup) xs) = isTuple tup && all (isFunCall args) xs
        isDictionary args _ = False
        
        isFunCall args (Apply (Fun x) vars) = and $ zipWith (==) vars (map Var [0..])
        isFunCall args x = isFun x


        isTuple x | "Prelude." `isPrefixOf` x = isTuple $ drop 8 x
                  | otherwise = "(" `isPrefixOf` x && ")" `isSuffixOf` x && all (== ',') (init $ tail x)



splitName :: String -> Dictionary
splitName name =  
    case length nams of
        5 -> Dictionary name (nams!!0) (TypeValue (nams!!3) (nams!!4)) (TypeClass (nams!!1) (nams!!2))
        -- 7 -> [Dictionary (nams!!0 ++ "." ++ nams!!1) (TypeValue (nams!!3) (nams!!4)) (TypeClass (nams!!1) (nams!!2))]
        _ -> error $ show ("Hill.Dictionary.getDictionaries",name)
    where
        nams = splitList "." name
