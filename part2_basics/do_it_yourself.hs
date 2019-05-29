{-
Функция одной переменной doItYourself выбирает наибольшее из переданного ей аргумента и числа 42, затем возводит результат выбора в куб и, наконец, вычисляет логарифм по основанию 2 от полученного числа. Эта функция реализована в виде:

doItYourself = f . g . h
Напишите реализации функций f, g и h. Постарайтесь сделать это в бесточечном стиле.

f = undefined

g = undefined

h = undefined
-}

module DoItYourself where

import Test.QuickCheck

doItYourself = f . g . h
    where 
        f = logBase 2
        g = (^3)
        h = max 42

test1 = doItYourself 64 == 18.0
test2 = doItYourself 128 == 21.0

main = mapM_ (quickCheck) [test1, test2]