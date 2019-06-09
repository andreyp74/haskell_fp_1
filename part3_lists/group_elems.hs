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
groupElems as = ge as [] [[]]
    where 
        ge :: Eq a => [a] -> [a] -> [[a]] -> [[a]]
        ge [] ys zss = ys:zss
        ge (x:xs) [] zss = ge xs [x] zss
        ge (x:xs) (y:ys) zss = 
            if x == y then ge xs (x:y:ys) zss
            else ge xs [x] ((y:ys):zss)

test1 = groupElems [] == [[]]
test2 = groupElems [1,2] == [[1],[2]]
test3 = groupElems [1,2,2,2,4] == [[1],[2,2,2],[4]]
test4 = groupElems [1,2,3,2,4] == [[1],[2],[3],[2],[4]]

main = mapM_ (quickCheck) [test1, test2, test3, test4]