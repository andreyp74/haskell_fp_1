{-
Используя однократный вызов свертки, реализуйте функцию evenOnly, которая выбрасывает из списка элементы, стоящие на нечетных местах, оставляя только четные.

GHCi> evenOnly [1..10]
[2,4,6,8,10]
GHCi> evenOnly ['a'..'z']
"bdfhjlnprtvxz"
-}

evenOnly :: [a] -> [a]
evenOnly = reverse . fst . foldl (\(s,c) x -> if c `mod` 2 == 0 then (x:s,1+c) else (s,1+c)) ([],1)


evenOnly' :: [a] -> [a]
evenOnly' = snd . foldr (\x (xs,ys) -> (x:ys,xs)) ([],[])


