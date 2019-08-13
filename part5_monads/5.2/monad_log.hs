{-
Реализованные ранее returnLog и bindLog позволяют объявить тип Log представителем класса Monad:

instance Monad Log where
    return = returnLog
    (>>=) = bindLog
Используя return и >>=, определите функцию execLoggersList

execLoggersList :: a -> [a -> Log a] -> Log a
которая принимает некоторый элемент, список функций с логированием и возвращает результат последовательного 
применения всех функций в списке к переданному элементу вместе со списком сообщений, которые возвращались данными функциями:

GHCi> execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]
Log ["added one","multiplied by 2","multiplied by 100"] 800
-}

module MonadLog where

import Control.Monad (ap,liftM)

data Log a = Log [String] a deriving (Eq, Show)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = \x -> Log [msg] (f x)

returnLog :: a -> Log a
returnLog = Log []

bindLog :: Log a -> (a -> Log b) -> Log b
bindLog (Log s1 x) f = let Log s2 y = f x
                       in Log (s1 ++ s2) y 

instance Functor Log where
    fmap = liftM

instance Applicative Log where
    pure = returnLog
    (<*>) = ap

instance Monad Log where
    return = returnLog
    (>>=) = bindLog

execLoggersList :: a -> [a -> Log a] -> Log a
execLoggersList x [] = return x
execLoggersList x (f:fs) = f x >>= (\y -> execLoggersList y fs)   

add1Log = toLogger (+1) "added one"
mult2Log = toLogger (* 2) "multiplied by 2"
test0 = (execLoggersList 3 [add1Log, mult2Log, \x -> Log ["multiplied by 100"] (x * 100)]) == Log ["added one","multiplied by 2","multiplied by 100"] 800
