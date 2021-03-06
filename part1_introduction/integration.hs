{-
Реализуйте функцию, находящую значение определённого интеграла от заданной функции \( f \) на заданном интервале \( [a,b] \) методом трапеций. (Используйте равномерную сетку; достаточно 1000 элементарных отрезков.)

integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b = undefined
GHCi> integration sin pi 0
-2.0
-}

module Integration where

    
integration :: (Double -> Double) -> Double -> Double -> Double
integration f a b | a == b    = 0
                  | a > b     = (- helper f b a n)
                  | otherwise = helper f a b n
                  where n = 1000

helper :: (Double -> Double) -> Double -> Double -> Double -> Double 
helper f a b n = h * (mid + (sum $ map f xi))
                 where 
                     h = (b - a)/n
                     mid = ((f a) + (f b))/2
                     xi = init $ tail $ [a,a+h..b]


integration' :: (Double -> Double) -> Double -> Double -> Double
integration' f a b = h/2 * (f a + f b + 2 * sumf):
    where
        n = 1000
        h = (b - a)/n
        sumf = sum $ map (\i -> f (a + i * h)) [1 .. (n - 1)]
