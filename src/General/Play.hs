
module General.Play where

import Control.Monad


-- THE CLASS
class Play on where
    replaceChildren :: on -> ([on], [on] -> on)


playDefault :: a -> ([b], [b] -> a)
playDefault x = ([], \[] -> x)


playOne :: (a -> a) -> a -> ([a], [a] -> a)
playOne part item = ([item], \[item] -> part item)



-- THE PLAYERS

mapUnder :: Play on => (on -> on) -> on -> on
mapUnder f x = f $ generate $ map (mapUnder f) current
    where (current, generate) = replaceChildren x


mapUnderM :: (Monad m, Play on) => (on -> m on) -> on -> m on
mapUnderM f x = mapM (mapUnderM f) current >>= f . generate
    where (current, generate) = replaceChildren x


fold :: Play on => ([res] -> res) -> (on -> res -> res) -> on -> res
fold merge gen x = gen x $ merge $ map (fold merge gen) current
    where current = fst $ replaceChildren x


composM :: (Monad m, Play on) => (on -> m on) -> on -> m on
composM f x = liftM generate $ mapM f current
    where (current, generate) = replaceChildren x




class Play with => PlayEx on with where
    replaceChildrenEx :: on -> ([with], [with] -> on)


playExDefault :: (Play on, PlayEx on with) => on -> ([with], [with] -> on)
playExDefault x = (concat currents, generate . zipWith ($) generates . divide currents)
    where
        divide [] [] = []
        divide (x:xs) ys = y1 : divide xs y2
            where (y1,y2) = splitAt (length x) ys
    
        (currents, generates) = unzip $ map replaceChildrenEx current
        (current, generate) = replaceChildren x


mapUnderEx :: PlayEx on with => (with -> with) -> on -> on
mapUnderEx f x = generate $ map (mapUnder f) current
    where (current, generate) = replaceChildrenEx x


instance (Play with, PlayEx on with) => PlayEx [on] with where
    replaceChildrenEx x = (concat currents, zipWith ($) generates . divide currents)
        where
            divide [] [] = []
            divide (x:xs) ys = y1 : divide xs y2
                where (y1,y2) = splitAt (length x) ys

            (currents, generates) = unzip $ map replaceChildrenEx x

