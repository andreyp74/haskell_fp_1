{-
Реализуйте функцию on3, имеющую семантику, схожую с on, но принимающую в качестве первого аргумента трехместную функцию
-}

module On3 where
import Test.QuickCheck


on3 :: (b -> b -> b -> c) -> (a -> b) -> a -> a -> a -> c
on3 op f x y z = op (f x) (f y) (f z)


test_sum3squares :: Bool
test_sum3squares = sum3squares 1 2 3 == 14
    where 
        sum3squares = (\x y z -> x+y+z) `on3` (^2)

main = quickCheck test_sum3squares