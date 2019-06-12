{-
Напишите функцию groupElems которая группирует одинаковые элементы в списке (если они идут подряд) 
и возвращает список таких групп.

GHCi> groupElems []
[]
GHCi> groupElems [1,2]
[[1],[2]]
GHCi> groupElems [1,2,2,2,4]
[[1],[2,2,2],[4]]
GHCi> groupElems [1,2,3,2,4]
[[1],[2],[3],[2],[4]]
Разрешается использовать только функции, доступные из библиотеки Prelude
-}

module GroupElems where

import Test.QuickCheck

groupElems :: Eq a => [a] -> [[a]]
groupElems [] = []
groupElems [x] = [[x]]
groupElems (x:xs) = 
    let s@(ys:yss) = groupElems xs 
    in
        if x == head ys 
        then (x:ys):yss 
        else [x]:s


test1 = groupElems [1,2] == [[1],[2]]
test2 = groupElems [1,2,2,2,4] == [[1],[2,2,2],[4]]
test3 = groupElems [1,2,3,2,4] == [[1],[2],[3],[2],[4]]

main = mapM_ (quickCheck) [test1, test2, test3]