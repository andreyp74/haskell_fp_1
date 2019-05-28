{-
В модуле Data.Function определена полезная функция высшего порядка

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on op f x y = f x `op` f y
Она принимает четыре аргумента: бинарный оператор с однотипными аргументами (типа b), функцию f :: a -> b, возвращающую значение типа b, и два значения типа a. 
Функция on применяет f дважды к двум значениям типа a и передает результат в бинарный оператор.

Используя on можно, например, записать функцию суммирования квадратов аргументов так:

sumSquares = (+) `on` (^2)

Функция multSecond, перемножающая вторые элементы пар, реализована следующим образом

multSecond = g `on` h
    g = undefined
    h = undefined

Напишите реализацию функций g и h

GHCi> multSecond ('A',2) ('E',7)
14

-}

module MultSecond where

import Data.Function

multSecond :: (a, Integer) -> (a, Integer) -> Integer
multSecond = g `on` h
    where 
        g = (*)
        h = snd

main = multSecond ('A',2) ('E',7)