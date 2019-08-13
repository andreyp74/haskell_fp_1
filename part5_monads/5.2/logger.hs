{-
Введём следующий тип:

data Log a = Log [String] a
Реализуйте вычисление с логированием, используя Log. Для начала определите функцию toLogger

toLogger :: (a -> b) -> String -> (a -> Log b)
которая превращает обычную функцию, в функцию с логированием:

GHCi> let add1Log = toLogger (+1) "added one"
GHCi> add1Log 3
Log ["added one"] 4

GHCi> let mult2Log = toLogger (* 2) "multiplied by 2"
GHCi> mult2Log 3
Log ["multiplied by 2"] 6
Далее, определите функцию execLoggers

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c

Которая принимает некоторый элемент и две функции с логированием. execLoggers возвращает результат последовательного применения функций к элементу и список сообщений, которые были выданы при применении каждой из функций:
GHCi> execLoggers 3 add1Log mult2Log
Log ["added one","multiplied by 2"] 8
-}

module Logger where


data Log a = Log [String] a deriving (Eq, Show)

toLogger :: (a -> b) -> String -> (a -> Log b)
toLogger f msg = \x -> Log [msg] (f x)

execLoggers :: a -> (a -> Log b) -> (b -> Log c) -> Log c
execLoggers x f g = let Log (s1:[]) y = f x
                        Log (s2:[]) z = g y
                    in Log (s1:s2:[]) z


add1Log = toLogger (+1) "added one"
test0 = (add1Log 3) == Log ["added one"] 4
mult2Log = toLogger (* 2) "multiplied by 2"
test1 = (mult2Log 3) == Log ["multiplied by 2"] 6

test2 = (execLoggers 3 add1Log mult2Log) == Log ["added one","multiplied by 2"] 8