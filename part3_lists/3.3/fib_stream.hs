{-
Реализуйте c использованием функции zipWith функцию fibStream, возвращающую бесконечный список чисел Фибоначчи.

GHCi> take 10 $ fibStream
[0,1,1,2,3,5,8,13,21,34]
-}

module FibStream where

import Test.QuickCheck


fibStream :: [Integer]
fibStream = helper 0   
    where
        helper n = fib n : helper (n+1)
        fib 0 = 0
        fib 1 = 1
        fib n = helper 1 0 2 n
            where
                helper acc' acc'' i n | i == n = acc' + acc''
                                      | i < n  = helper (acc' + acc'') acc' (i + 1) n

fibStream' :: [Integer]
fibStream' = 0 : 1 : zipWith (+) fibStream (tail fibStream)

main = quickCheck $ (take 10 $ fibStream) == [0,1,1,2,3,5,8,13,21,34]