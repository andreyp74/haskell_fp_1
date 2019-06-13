{-
Воспользовавшись функциями map и concatMap, определите функцию perms, которая возвращает все перестановки, 
которые можно получить из данного списка, в любом порядке.

GHCi> perms [1,2,3]
[[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,1,2],[3,2,1]]
Считайте, что все элементы в списке уникальны, и что для пустого списка имеется одна перестановка.
-}

module Perms where

import Test.QuickCheck

perms :: [a] -> [[a]]
perms [] = [[]]
perms [x] = [[x]]
perms (x:y:[]) = [[x,y],[y,x]]
perms xs = helper xs []
    where
        helper [] _ = []
        helper (x:xs) ys = (map (x:) $ perms (ys ++ xs)) ++ helper xs (x:ys)


test1 = perms [1,2,3] == [[1,2,3],[1,3,2],[2,1,3],[2,3,1],[3,2,1],[3,1,2]]

main = mapM_ (quickCheck) [test1]