{-
Реализуйте функцию filterDisj, принимающую два унарных предиката и список, и возвращающую список элементов, 
удовлетворяющих хотя бы одному из предикатов.

GHCi> filterDisj (< 10) odd [7,8,10,11,12]
[7,8,11]
-}

module FilterDisj where

filterDisj :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj _ _ [] = []
filterDisj p1 p2 (x:xs)
        | p1 x || p2 x = x : filterDisj p1 p2 xs
        | otherwise = filterDisj p1 p2 xs

filterDisj' :: (a -> Bool) -> (a -> Bool) -> [a] -> [a]
filterDisj' p1 p2 = filter (\x -> p1 x || p2 x)