{-
И снова монада Log. Пусть теперь оператор >>= будет добавлять сообщения как в начало списка, так и в конец. 

Выберите верные утверждения относительно выполнения законов для монады с новым поведением оператора >>=.
-}

module MonadLaws3 where

import Control.Monad (ap,liftM)

data Log a = Log [String] a deriving (Eq, Show)

instance Functor Log where
    fmap = liftM

instance Applicative Log where
    pure = Log []
    (<*>) = ap

instance Monad Log where
    return = pure
    (Log s1 x) >>= f = let Log s2 y = f x in Log (s2 ++ s1 ++ s2) y


test1 = (return 1 >>= \x -> Log ["plus one"] (x + 1)) == (\x -> Log ["plus one"] (x + 1)) 1
test2 = (Log ["one"] 1 >>= return) == Log ["one"] 1
test3 = let k = \x -> Log ["plus one"] (x + 1)
            k' = \x -> Log ["pruduct 2"] (x * 2)
            left = (Log ["one"] 1) >>= k >>= k'
            right =  (Log ["one"] 1) >>= (\y -> k y >>= k')
        in left == right 

testLog = (test1 == False) && (test2 == True) && (test3 == False)