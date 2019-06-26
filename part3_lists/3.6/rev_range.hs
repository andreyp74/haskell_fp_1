{-
Используя unfoldr, реализуйте функцию, которая возвращает в обратном алфавитном порядке список символов, попадающих в заданный парой диапазон. Попадание символа x в диапазон пары (a,b) означает, что x >= a и x <= b.

revRange :: (Char,Char) -> [Char]
revRange = unfoldr g 
  where g = undefined
GHCi> revRange ('a','z')
"zyxwvutsrqponmlkjihgfedcba"
-}

module RevRange where

import Data.Char
import Test.QuickCheck

unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr f ini = helper (f ini) where
    helper (Just (x, ini')) = x : unfoldr f ini'
    helper Nothing          = [] 


revRange :: (Char,Char) -> [Char]
revRange (a,b) | a <= b = unfoldr g b
               | otherwise = []
    where g = \x -> if x<=b && x>=a then Just (x,chr(ord x-1)) else Nothing


test1 = quickCheck $ revRange ('a','z') == "zyxwvutsrqponmlkjihgfedcba"


