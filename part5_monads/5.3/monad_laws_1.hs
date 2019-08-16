
{-
Вспомним тип Log

data Log a = Log [String] a
который мы сделали монадой в предыдущем модуле. Функция return для Log оборачивает переданное значение в лог с пустым списком сообщений. Оператор >>= возвращает лог с модифицированным значением и новым списком сообщений, который состоит из прежнего списка и добавленного в конец списка сообщений, полученных при модификации значения.

Пусть теперь функция return будет оборачивать переданное значение в список, содержащий одно стандартное сообщение "Log start".

Выберите верные утверждения относительно выполнения законов для монады с новым поведением функции return
-}



module MonadLaws1 where

import Control.Monad (ap,liftM)

data Log a = Log [String] a deriving (Eq, Show)

instance Functor Log where
    fmap = liftM

instance Applicative Log where
    pure = Log ["Log start"]
    (<*>) = ap

instance Monad Log where
    return = pure
    (Log s1 x) >>= f = let Log s2 y = f x in Log (s2 ++ s1) y


test1 = (return 1 >>= \x -> Log ["plus one"] (x + 1)) == (\x -> Log ["plus one"] (x + 1)) 1
test2 = (Log ["one"] 1 >>= return) == Log ["one"] 1
test3 = let k = \x -> Log ["plus one"] (x + 1)
            k' = \x -> Log ["pruduct 2"] (x * 2)
            left = (Log ["one"] 1) >>= k >>= k'
            right =  (Log ["one"] 1) >>= (\y -> k y >>= k')
        in left == right 

testLog = (test1 == False) && (test2 == False) && (test3 == True)