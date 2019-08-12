{-
Ниже приведено определение класса MapLike типов, похожих на тип Map. 
Определите представителя MapLike для типа ListMap, определенного ниже как список пар ключ-значение. Для каждого ключа должно храниться не больше одного значения. 
Функция insert заменяет старое значение новым, если ключ уже содержался в структуре.
-}

module ListMap where

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

newtype ListMap k v = ListMap { getListMap :: [(k,v)] }
    deriving (Eq,Show)

instance MapLike ListMap where
    empty = ListMap { getListMap=[] } 

    lookup _ (ListMap []) = Nothing
    lookup key (ListMap ((k,v):xs)) | key == k = Just v
                                    | otherwise = lookup key (ListMap xs) 

    insert key value (ListMap dict) = ListMap $ (key,value):(getListMap $ delete key (ListMap dict))

    delete key (ListMap dict) = deleteHelper key (ListMap dict) (ListMap [])
        where 
            deleteHelper _  (ListMap []) (ListMap newDict) = ListMap $ reverse newDict
            deleteHelper key (ListMap ((k,v):xs)) (ListMap newDict) | key /= k = deleteHelper key (ListMap xs) (ListMap $ (k,v):newDict)
                                                                    | otherwise = deleteHelper key (ListMap xs) (ListMap newDict)


test0 = lookup 2 ((fromList [(1,"one"),(2,"two"),(3,"three")]) :: ListMap Int String) == Just "two"
test1 = lookup 4 ((fromList [(1,"one"),(2,"two"),(3,"three")]) :: ListMap Int String) == Nothing
test2 = lookup 4 (insert 4 "four" ((fromList [(1,"one"),(2,"two"),(3,"three")]) :: ListMap Int String)) == Just "four"
test3 = lookup 1 (empty:: ListMap Int String) == Nothing
test4 = lookup 2 (delete 2 ((fromList [(1,"one"),(2,"two"),(3,"three")]) :: ListMap Int String)) == Nothing
test5 = getListMap (fromList [(1,"one"),(2,"two"),(3,"three")] :: ListMap Int String) == [(1,"one"),(2,"two"),(3,"three")]
test6 = getListMap (insert 4 "four" (fromList [(1,"one"),(2,"two"),(3,"three")] :: ListMap Int String)) == [(4, "four"),(1,"one"),(2,"two"),(3,"three")]
test7 = getListMap (delete 2 (fromList [(1,"one"),(2,"two"),(3,"three")] :: ListMap Int String)) == [(1,"one"),(3,"three")]
test8 = getListMap (delete 1 (fromList [(1,"one"),(2,"two"),(3,"three")] :: ListMap Int String)) == [(2,"two"),(3,"three")]
test9 = getListMap (delete 3 (fromList [(1,"one"),(2,"two"),(3,"three")] :: ListMap Int String)) == [(1,"one"),(2,"two")]
test10 = lookup 1 (insert 1 "one" (empty:: ListMap Int String)) == Just "one"
test11 = getListMap (delete 1 (empty:: ListMap Int String)) == []

test12 = getListMap (insert 2 "TWO" ((fromList [(1,"one"),(2,"two"),(3,"three")]) :: ListMap Int String)) == [(2,"TWO"),(1,"one"),(3,"three")]


testAll = test0 && test1 && test2 && test3 && test4 && test5 && test6 && test7 && test8 && test9 && test10 && test11 && test12