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

data Odd = Odd Integer 
    deriving (Eq, Show)

addEven :: Integer -> Odd -> Odd
addEven m (Odd n) | m `mod` 2 == 0 = Odd (n + m)
                  | otherwise      = error "addEven: second parameter cannot be odd"

instance Enum Odd where  
    succ = addEven 2
    pred = addEven (-2)
    toEnum n = (Odd $ toInteger n)    
    fromEnum (Odd n) = fromIntegral n
    enumFrom from = xs where xs = succ from : xs 
    enumFromThen (Odd from) (Odd then') = xs 
        where 
            delta = then' - from
            next = addEven delta (Odd from)
            xs = next : xs
    enumFromTo from (Odd to) = takeWhile (\(Odd x) -> x <= to) $ enumFrom from 
    enumFromThenTo from then' (Odd to) = takeWhile (\(Odd x) -> x <= to) $ enumFromThen from then'

             
