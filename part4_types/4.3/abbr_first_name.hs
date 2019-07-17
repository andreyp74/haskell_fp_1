{-
Определить функцию abbrFirstName, которая сокращает имя до первой буквы с точкой, то есть, если имя было "Ivan", 
то после применения этой функции оно превратится в "I.". Однако, если имя было короче двух символов, то оно не меняется.
-}

data Person = Person { firstName :: String, lastName :: String, age :: Int }

abbrFirstName :: Person -> Person
abbrFirstName p = p { firstName = firstTwo (firstName p) }
    where 
        firstTwo s | length s <= 2 = s
                   | otherwise     = (take 1 s) ++ "."

