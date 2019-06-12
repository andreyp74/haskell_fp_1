{-
Напишите функцию squares'n'cubes, принимающую список чисел, 
и возвращающую список квадратов и кубов элементов исходного списка.
GHCi> squares'n'cubes [3,4,5]
[9,27,16,64,25,125]
-}

module Squares'n'cubes where

import Test.QuickCheck

squares'n'cubes :: Num a => [a] -> [a]
squares'n'cubes = concatMap (\x -> [x*x,x*x*x])

test = quickCheck $ squares'n'cubes [3,4,5] == [9,27,16,64,25,125]