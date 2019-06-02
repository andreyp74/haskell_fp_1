{-
Пусть существуют два класса типов KnownToGork и KnownToMork, которые предоставляют методы stomp (stab) и 
doesEnrageGork (doesEnrageMork) соответственно:

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

Класс типов KnownToGorkAndMork является расширением обоих этих классов, предоставляя дополнительно 
метод stompOrStab:

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a

Задайте реализацию по умолчанию метода stompOrStab, которая вызывает метод stomp, если переданное ему значение 
приводит в ярость Морка; вызывает stab, если оно приводит в ярость Горка и вызывает сначала stab, а потом stomp, 
если оно приводит в ярость их обоих. Если не происходит ничего из вышеперечисленного, метод должен возвращать 
переданный ему аргумент.

-}

module KnownTo where

class KnownToGork a where
    stomp :: a -> a
    doesEnrageGork :: a -> Bool

class KnownToMork a where
    stab :: a -> a
    doesEnrageMork :: a -> Bool

class (KnownToGork a, KnownToMork a) => KnownToGorkAndMork a where
    stompOrStab :: a -> a
    stompOrStab a | enrageMork && enrageGork = stomp $ stab a
                  | enrageMork = stomp a
                  | enrageGork = stab a
                  | otherwise = a
            where 
                enrageGork = doesEnrageGork a
                enrageMork = doesEnrageMork a