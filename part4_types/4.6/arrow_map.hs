{-
Реализуйте представителя MapLike для типа ArrowMap, определенного ниже.
-}

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
    insert key value (ArrowMap f) = ArrowMap (\k -> if k == key then Just value else f k)
    lookup key (ArrowMap f) = f key  
    delete key (ArrowMap f) = ArrowMap (\k -> if k == key then Nothing else f k)

test0 = lookup 1 (insert 1 "one" (empty :: ArrowMap Int String)) == Just "one"
test1 = lookup 1 (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String))) == Just "one"
test2 = lookup 2 (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String))) == Just "two"
test3 = lookup 3 (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String))) == Nothing
test4 = lookup 1 (insert 3 "three" (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String)))) == Just "one"
test5 = lookup 2 (insert 3 "three" (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String)))) == Just "two"
test6 = lookup 3 (insert 3 "three" (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String)))) == Just "three"
test7 = lookup 1 (delete 1 (insert 3 "three" (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String))))) == Nothing
test8 = lookup 2 (delete 2 (insert 3 "three" (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String))))) == Nothing
test9 = lookup 3 (delete 3 (insert 3 "three" (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String))))) == Nothing
test10 = lookup 2 (delete 1 (insert 3 "three" (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String))))) == Just "two"
test11 = lookup 3 (delete 2 (insert 3 "three" (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String))))) == Just "three"
test12 = lookup 1 (delete 3 (insert 3 "three" (insert 2 "two" (insert 1 "one" (empty :: ArrowMap Int String))))) == Just "one"

testAll = test0 && test1 && test2 && test3 && test4 && test5 && test6 && test7 && test8 && test9 && test10 && test11 && test12