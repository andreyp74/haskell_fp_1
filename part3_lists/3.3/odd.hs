{-
Пусть задан тип Odd нечетных чисел следующим образом:

data Odd = Odd Integer 
  deriving (Eq, Show)
Сделайте этот тип представителем класса типов Enum.

GHCi> succ $ Odd (-100000000000003)
Odd (-100000000000001)
Конструкции с четным аргументом, типа Odd 2, считаются недопустимыми и не тестируются.

Примечание. Мы еще не знакомились с объявлениями пользовательских типов данных, однако, скорее всего, приведенное объявление не вызовет сложностей. 
Здесь объявляется тип данных Odd с конструктором Odd. Фактически это простая упаковка для типа Integer. Часть deriving (Eq, Show) указывает компилятору, 
чтобы он автоматически сгенерировал представителей соответствующих классов типов для нашего типа (такая возможность имеется для ряда стандартных классов типов). 
Значения типа Odd можно конструировать следующим образом:

GHCi> let x = Odd 33
GHCi> x
Odd 33
и использовать конструктор данных Odd в сопоставлении с образцом:

addEven :: Odd -> Integer -> Odd
addEven (Odd n) m | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"
-}

module Odd where

import Test.QuickCheck

data Odd = Odd Integer 
    deriving (Eq, Show)

addEven :: Integer -> Odd -> Odd
addEven m (Odd n) | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"

range :: (Odd -> Odd) -> Odd -> [Odd]                   
range nextf from = from:(range nextf (nextf from))


instance Enum Odd where  
    succ = addEven 2
    pred = addEven (-2)
    toEnum n = Odd $ toInteger n    
    fromEnum (Odd n) = fromIntegral n
    enumFrom from = range succ from
    enumFromThen from@(Odd f) (Odd t) = range (addEven (t - f)) from
    enumFromTo from (Odd to) = takeWhile (\(Odd x) -> x <= to) $ enumFrom from 
    enumFromThenTo from@(Odd f) then_@(Odd t) (Odd to) | f > t    = takeWhile (\(Odd x) -> x >= to) $ enumFromThen from then_
                                                       | f < t    = takeWhile (\(Odd x) -> x <= to) $ enumFromThen from then_
                                                       | otherwise = takeWhile (\(Odd x) -> x == to) $ enumFromThen from then_

{- Более короткий вариант

instance Enum Odd' where  
    succ = addEven 2
    pred = addEven (-2)
    toEnum n = Odd' $ toInteger n    
    fromEnum (Odd' n) = fromIntegral n
    enumFrom (Odd' n) = map Odd' [n,n+2..]
    enumFromThen (Odd' n) (Odd' k) = map Odd' [n,k..]
    enumFromTo (Odd' n) (Odd' m) = map Odd' [n,n+2..m]
    enumFromThenTo (Odd' n) (Odd' k) (Odd' m) = map Odd' [n,k..m]
 -}

-- Большое число, которое не поместится в Int
baseVal = 9900000000000000000

-- Генератор значений для тестирования
testVal n = Odd $ baseVal + n
-- для проверки самих тестов. Тесты с 0..3 не должны выполняться
-- testVal = id

test0 = succ (testVal 1) == (testVal 3)
test1 = pred (testVal 3) == (testVal 1)
-- enumFrom
test2 = take 4 [testVal 1 ..] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- enumFromTo
-- -- По возрастанию
test3 = take 9 [testVal 1..testVal 7] == [testVal 1,testVal 3,testVal 5,testVal 7]
-- -- По убыванию
test4 = take 3 [testVal 7..testVal 1] == []
-- enumFromThen
-- -- По возрастанию
test5 = take 4 [testVal 1, testVal 5 ..] == [testVal 1,testVal 5,testVal 9,testVal 13]
-- -- По убыванию
test6 = take 4 [testVal 5, testVal 3 ..] == [testVal 5,testVal 3,testVal 1,testVal (-1)]
-- enumFromThenTo
-- -- По возрастанию
test7 = [testVal 1, testVal 5 .. testVal 11] == [testVal 1,testVal 5,testVal 9]
-- -- По убыванию
test8 = [testVal 7, testVal 5 .. testVal 1] == [testVal 7,testVal 5,testVal 3,testVal 1]
-- -- x1 < x3 && x1 > x2
test9 = [testVal 7, testVal 5 .. testVal 11] == []
-- -- x1 > x3 && x1 < x2
test10 = [testVal 3, testVal 5 .. testVal 1] == []

test11 = take 4 [testVal 5, testVal 5 .. ] == replicate 4 (testVal 5)
-- Неочевидный тест, но такое поведение ожидается 
test12 = take 4 [testVal 5, testVal 5 .. testVal 11] == []
test13 = take 4 [testVal 5, testVal 5 .. testVal 5] == replicate 4 (testVal 5)
test14 = [testVal 5, testVal 5 .. testVal 3] == []
test15 = [testVal 5, testVal 1 .. testVal 5] == [testVal 5]
test16 = toEnum (fromEnum (Odd 3)) == Odd 3
-- Это сомнительный тест. Скорее всего, его нет на stepik
--test17 = fromEnum(Odd 3) + 1 == fromEnum(Odd 5)

testList = [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16]
allTests = zip [0..] testList
-- Список тестов с ошибками
badTests = map fst $ filter (not . snd) allTests

allTestsQC = mapM_ (quickCheck) [test0, test1, test2, test3, test4, test5, test6, test7, test8, test9, test10, test11, test12, test13, test14, test15, test16]

             
