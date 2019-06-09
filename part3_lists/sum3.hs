{-
Составьте список сумм соответствующих элементов трех заданных списков. Длина результирующего списка 
должна быть равна длине самого длинного из заданных списков, при этом «закончившиеся» списки не 
должны давать вклада в суммы.

GHCi> sum3 [1,2,3] [4,5] [6]
[11,7,3]
-}


module Sum3 where

sum3 :: Num a => [a] -> [a] -> [a] -> [a]
sum3 (x:xs) (y:ys) (z:zs) = (x+y+z):sum3 xs ys zs
sum3 [] (y:ys) (z:zs) = sum2 ys zs
sum3 (x:xs) [] (z:zs) = sum2 xs zs
sum3 (x:xs) (y:ys) [] = sum2 xs ys
    where 
        sum2 (x:xs) (y:ys) = (x + y):sum2 xs ys
        sum2 _ (y:ys) = ys
        sum2 (x:xs) _ = xs 