{-

Определите представителя класса Functor для бинарного дерева, в каждом узле которого хранятся элементы типа Maybe:

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving Show

GHCi> words <$> Leaf Nothing
Leaf Nothing

GHCi> words <$> Leaf (Just "a b")
Leaf (Just ["a","b"])
-}

module FunctorTree where

data Tree a = Leaf (Maybe a) | Branch (Tree a) (Maybe a) (Tree a) deriving (Eq, Show)

instance Functor Tree where
    fmap f (Leaf x) = Leaf (fmap f x)
    fmap f (Branch l x r) = Branch (fmap f l) (fmap f x) (fmap f r)

test0 = (words <$> Leaf Nothing) == Leaf Nothing
test1 = (words <$> Leaf (Just "a b")) == Leaf (Just ["a","b"])