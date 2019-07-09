{-
Пусть есть список положительных достоинств монет coins, отсортированный по возрастанию. Воспользовавшись механизмом генераторов списков, 
напишите функцию change, которая разбивает переданную ей положительную сумму денег на монеты достоинств из списка coins всеми возможными способами. 
Например, если coins = [2, 3, 7]:

GHCi> change 7
[[2,2,3],[2,3,2],[3,2,2],[7]]

Примечание. Порядок монет в каждом разбиении имеет значение, то есть наборы [2,2,3] и [2,3,2] — различаются.
Список coins определять не надо.
-}


module Change where

import Data.List

coins :: (Num a) => [a]
coins = [2,3,7]

change :: (Ord a, Num a) => a -> [[a]]
change s = nub $ concatMap 
 $ filter (\xs -> sum(xs) == s) [x++y | x <- genLists s coins, y <- genLists s coins, x < y]
    where 
        replicateSerial x n = (replicate n x) : replicateSerial x (n + 1)
        replicateSerialWhile k m = takeWhile (\xs -> sum(xs) <= m)  $ replicateSerial k 0
        genLists s' = concatMap (\x -> replicateSerialWhile x s')