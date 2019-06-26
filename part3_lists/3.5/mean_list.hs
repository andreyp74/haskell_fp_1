{-
Реализуйте функцию meanList, которая находит среднее значение элементов списка, используя однократный вызов функции свертки.

GHCi> meanList [1,2,3,4]
2.5
Постобработка считается допустимой, то есть предполагаемая реализация функции meanList имеет вид

meanList = someFun . foldr someFoldingFun someIni
-}

meanList :: [Double] -> Double
meanList xs = let (s,c) = foldr (\x (s,c) -> (x+s,1+c)) (0,0) xs in s/c