{-
Ниже приведено определение класса MapLike типов, похожих на тип Map. 
Определите представителя MapLike для типа ListMap, определенного ниже как список пар ключ-значение. Для каждого ключа должно храниться не больше одного значения. 
Функция insert заменяет старое значение новым, если ключ уже содержался в структуре.
-}

module MapLike where

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

    insert key value (ListMap dict) = ListMap $ (key,value):dict

    delete key (ListMap dict) = deleteHelper key (ListMap dict) (ListMap [])
        where 
            deleteHelper _  (ListMap []) (ListMap newDict) = ListMap $ reverse newDict
            deleteHelper key (ListMap ((k,v):xs)) (ListMap newDict) | key /= k = deleteHelper key (ListMap xs) (ListMap $ (k,v):newDict)
                                                                    | otherwise = deleteHelper key (ListMap xs) (ListMap newDict)

--test0 = MapLike.fromList [(0,1),(2,3),(4,5)] (ListMap [])