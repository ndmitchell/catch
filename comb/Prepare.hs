
module Prepare(prepare) where

import Yhc.Core


prepare :: Core -> Core
prepare core = core{coreDatas = map prepareData (coreDatas core)}


prepareData :: CoreData -> CoreData
prepareData x = x{coreDataCtors = map (prepareCtor rec) (coreDataCtors x)}


prepareCtor :: String -> CoreCtor -> CoreCtor
prepareCtor rec (CoreCtor name fields) = CoreCtor name (zipWith f [0..] fields)
    where
        f n (typ,Nothing) | name == "Prelude.:" = f n (typ, Just $ ["hd","tl"] !! n)
                          | otherwise = f n (typ, Just $ 
        
        
        f n (typ,Just q) = 
    
        rec
    
        f n x = x -- error $ show x
        
        b (Prelude.[] b)
        

{-

data CoreCtor = CoreCtor {
coreCtorName :: CoreCtorName
coreCtorFields :: [(String, Maybe String)]
}
-}
