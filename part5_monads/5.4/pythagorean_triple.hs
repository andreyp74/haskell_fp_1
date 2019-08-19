{-
Используя монаду списка и do-нотацию, реализуйте функцию

pythagoreanTriple :: Int -> [(Int, Int, Int)]

которая принимает на вход некоторое число x и возвращает список троек (a,b,c), таких что

a^2+b^2=c^2,a>0,b>0,c>0,c≤x,a<b  

Число x может быть ≤0 , на таком входе должен возвращаться пустой список.

GHCi> pythagoreanTriple 5
[(3,4,5)]

GHCi> pythagoreanTriple 0
[]

GHCi> pythagoreanTriple 10
[(3,4,5),(6,8,10)]
-}

module PythagoreanTriple where

pythagoreanTriple :: Int -> [(Int, Int, Int)]
pythagoreanTriple x | x <= 0 = []
                    | otherwise = do 
                a <- [0..x]
                b <- [0..x]
                c <- [0..x]
                True <- return (a < b) 
                True <- return (x >= c)
                True <- return (a > 0)
                True <- return (b > 0)
                True <- return (c > 0)
                True <- return (a^2 + b^2 == c^2)
                return (a,b,c)



test1 = (pythagoreanTriple 5) == [(3,4,5)]
test2 = (pythagoreanTriple 0) == []
test3 = (pythagoreanTriple 10) == [(3,4,5),(6,8,10)]

testAll = test1 && test2 && test3