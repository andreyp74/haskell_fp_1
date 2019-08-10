{-
Определите представителя класса Functor для типов данных Entry и Map. Тип Map представляет словарь, ключами которого являются пары:

data Entry k1 k2 v = Entry (k1, k2) v  deriving Show
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving Show

В результате должно обеспечиваться следующее поведение: fmap применяет функцию к значениям в словаре, не изменяя при этом ключи.

GHCi> fmap (map toUpper) $ Map []
Map []

GHCi> fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]
Map [Entry (0,0) "ORIGIN",Entry (800,0) "RIGHT CORNER"]
-}

module FunctorEntryMap where

import Data.Char(toUpper)

data Entry k1 k2 v = Entry (k1, k2) v  deriving (Eq, Show)
data Map k1 k2 v = Map [Entry k1 k2 v]  deriving (Eq, Show)

instance Functor (Entry k1 k2) where
    fmap f (Entry (x,y) v) = Entry (x,y) (f v)

instance Functor (Map k1 k2) where
    fmap f (Map []) = Map []
    fmap f (Map (x:xs)) = let 
                            Map ys = fmap f (Map xs)
                            y = fmap f x
                          in Map (y:ys)

empty = [] :: [Entry Int Int String]
test0 = (fmap (map toUpper) $ Map empty) == Map empty
test1 = (fmap (map toUpper) $ Map [Entry (0, 0) "origin", Entry (800, 0) "right corner"]) == Map [Entry (0,0) "ORIGIN",Entry (800,0) "RIGHT CORNER"]