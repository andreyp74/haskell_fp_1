{-
Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа.

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = undefined
GHCi> sum'n'count (-39)
(12,2)
-}

module SumNCount where

import Data.Char

sum'n'count' :: Integer -> (Integer, Integer)
sum'n'count' x = 
    let 
        digits = show $ abs x 
        len = toInteger $ length digits
        digSum = toInteger $ foldl (+) 0 $ map digitToInt digits
    in (digSum, len)


sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0,1)
sum'n'count n = let an = abs(n)
                    dpows = map (\x -> 10^x) [0..]
                    rl = reverse $ takeWhile (\x -> an `div` x > 0) dpows
                    coeffs _ []     ys = ys
                    coeffs z (x:xs) ys = let v = z `div` x
                                         in coeffs (z - v*x) xs (v:ys)
                in (sum $ coeffs an rl [], toInteger $ length rl)  