{-
Напишите функцию с сигнатурой:

avg :: Int -> Int -> Int -> Double
вычисляющую среднее значение переданных в нее аргументов:

GHCi> avg 3 4 8
5.0
-}

module Avg where

avg :: Int -> Int -> Int -> Double
avg x y z = (xi + yi + zi) / 3.0
    where
        xi = fromIntegral x
        yi = fromIntegral y
        zi = fromIntegral z