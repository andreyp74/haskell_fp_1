{-
Реализуйте функцию isPalindrome, которая определяет, является ли переданный ей список палиндромом.

GHCi> isPalindrome "saippuakivikauppias"
True
GHCi> isPalindrome [1]
True
GHCi> isPalindrome [1, 2]
False
-}

module IsPalindrome where

import Test.QuickCheck

isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = True
isPalindrome [x] = True
isPalindrome (x:xs) = 
    if x /= last xs then False 
    else isPalindrome $ init xs

test1 = isPalindrome [1] == True
test2 = isPalindrome [1, 2] == False
test3 = isPalindrome "saippuakivikauppias" == True

main = mapM_ (quickCheck) [test1, test2, test3]