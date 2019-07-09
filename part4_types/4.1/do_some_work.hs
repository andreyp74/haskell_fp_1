{-
Пусть объявлен следующий тип данных:

data Result = Fail | Success

И допустим определен некоторый тип данных SomeData и некоторая функция
doSomeWork :: SomeData -> (Result,Int)
возвращающая результат своей работы и либо код ошибки в случае неудачи, либо 0 в случае успеха.
Определите функцию processData, которая вызывает doSomeWork и возвращает строку "Success" в случае ее успешного завершения, либо строку "Fail: N" в случае неудачи, 
где N — код ошибки.
-}

data Result = Fail | Success

doSomeWork :: SomeData -> (Result,Int)

processData :: SomeData -> String
processData x = case doSomeWork x of
    (_, 0) -> "Success"
    (_, x) -> "Fail: " ++ show x 