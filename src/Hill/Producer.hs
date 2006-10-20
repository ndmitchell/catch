
module Hill.Producer(producer) where

import Hill.Type
import qualified Data.Map as Map
import Control.Monad.State


type ProducerMonad spec a = State (Store spec) a

data Store spec = Store {stateId :: Int
                        ,stateTable :: Map.Map spec FuncName
                        ,stateTodo :: [Func]
                        ,stateDone :: [Func]
                        }


producer :: Ord spec =>
            Hill -> -- to work in
            [Func] -> -- list of functions initially in pending
            ((spec -> ProducerMonad spec FuncName) -> Func -> ProducerMonad spec Func) -> -- processor
            (spec -> Int -> Func) -> -- generator
            ([Func], [(spec, FuncName)])
producer hill todo processor generator = (reverse $ stateDone state, Map.toList $ stateTable state)
    where
        state = execState driver (Store (calcUnique hill) Map.empty todo [])
        
        
        -- driver :: State (Store spec) () -> State (Store spec) ()
        driver = do
            s <- get
            if null $ stateTodo s then return () else do
                let donow = head $ stateTodo s
                modify $ \s -> s{stateTodo = tail $ stateTodo s}
                res <- processor ask donow
                modify $ \s -> s{stateDone = res : stateDone s}
                driver
        
        
        -- ask :: spec -> State (Store spec) Func
        ask spec = do
            s <- get
            case Map.lookup spec (stateTable s) of
                Just y -> return y
                Nothing -> do
                    let newfunc = generator spec (stateId s)
                        newname = funcName newfunc
                    put s{stateTodo = newfunc : stateTodo s
                         ,stateTable = Map.insert spec newname (stateTable s)}
                    return newname
