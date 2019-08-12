{-
Реализуйте представителя MapLike для типа ArrowMap, определенного ниже.
-}

module ArrowMap where

import Prelude hiding (lookup)
import qualified Data.List as L

class MapLike m where
    empty :: m k v
    lookup :: Ord k => k -> m k v -> Maybe v
    insert :: Ord k => k -> v -> m k v -> m k v
    delete :: Ord k => k -> m k v -> m k v
    fromList :: Ord k => [(k,v)] -> m k v
    fromList [] = empty
    fromList ((k,v):xs) = insert k v (fromList xs)

newtype ArrowMap k v = ArrowMap { getArrowMap :: k -> Maybe v }

instance MapLike ArrowMap where
    empty = ArrowMap (\_ -> Nothing)

    insert key value (ArrowMap f) = ArrowMap (\k -> if key == k then (Just value) else f key) 

    lookup key (ArrowMap f) = f key

    delete key (ArrowMap f) = undefined


test0 = lookup 1 (empty :: ArrowMap Int String) == Nothing
test1 = lookup 1 (insert 1 "one" (empty :: ArrowMap Int String)) == Just "one"
test2 = lookup 2 (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String))) == Just "two"
test3 = lookup 1 (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String))) == Just "one"
test4 = lookup 2 (insert 3 "three" (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String)))) == Just "two"
