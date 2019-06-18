{-
Реализация функции для вычисления числа Фибоначчи, основанная на прямом рекурсивном определении, крайне 
неэффективна - количество вызовов функции растет экспоненциально с ростом значения аргумента. 
GHCi позволяет отслеживать использование памяти и затраты времени на вычисление выражения, для этого следует 
выполнить команду :set +s:

GHCi> :set +s
GHCi> fibonacci 30
832040
(8.36 secs, 298293400 bytes)

С помощью механизма аккумуляторов попробуйте написать более эффективную реализацию, имеющую линейную сложность 
(по числу рекурсивных вызовов). Как и в предыдущем задании, функция должна быть определена для всех целых чисел.
-}

module Fibonacci where

fibonacci :: Integer -> Integer
fibonacci n | n == 0 = 0
            | n == 1 = 1
            | n == (-1) = 1
            | n == (-2) = (-1)
            | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = ((-1) ^ ((-n) + 1)) * fibonacci (-n)


fibonacci' :: Integer -> Integer
fibonacci' 0 = 0
fibonacci' 1 = 1
fibonacci' (-1) = 1
fibonacci' (-2) = (-1)
fibonacci' n | n > 0 = helper 1 0 2 n
             | n < 0 = ((-1) ^ ((-n) + 1)) * fibonacci' (-n)
    where 
        helper :: Integer -> Integer -> Integer -> Integer -> Integer
        helper acc' acc'' i n | i == n = acc' + acc''
                              | i < n  = helper (acc' + acc'') acc' (i + 1) n
            
