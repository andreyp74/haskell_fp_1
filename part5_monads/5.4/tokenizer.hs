{-
Рассмотрим язык арифметических выражений, которые состоят из чисел, скобок, операций сложения и вычитания. Конструкции данного языка можно представить 
следующим типом данных:

data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)
Реализуйте лексер арифметических выражений. Для начала реализуйте следующую функцию:

asToken :: String -> Maybe Token

Она проверяет, является ли переданная строка числом (используйте функцию isDigit из модуля Data.Char), знаком "+" или "-", открывающейся или закрывающейся скобкой. 
Если является, то она возвращает нужное значение обёрнутое в Just, в противном случае - Nothing:

GHCi> asToken "123"
Just (Number 123)

GHCi> asToken "abc"
Nothing

Далее, реализуйте функцию tokenize:

tokenize :: String -> Maybe [Token]
Функция принимает на вход строку и если каждое слово является корректным токеном, то она возвращает список этих токенов, завёрнутый в Just. 
В противном случае возвращается Nothing. 

Функция должна разбивать входную строку на отдельные слова по пробелам (используйте библиотечную функцию words). 
Далее, полученный список строк должен быть свёрнут с использованием функции asToken и свойств монады Maybe:

GHCi> tokenize "1 + 2"
Just [Number 1,Plus,Number 2]

GHCi> tokenize "1 + ( 7 - 2 )"
Just [Number 1,Plus,LeftBrace,Number 7,Minus,Number 2,RightBrace]

GHCi> tokenize "1 + abc"
Nothing
Обратите внимание, что скобки отделяются пробелами от остальных выражений!
-}

module Tokenizer where

import Data.Char(isDigit)
import Data.List(all)


data Token = Number Int | Plus | Minus | LeftBrace | RightBrace 
    deriving (Eq, Show)


asToken :: String -> Maybe Token
asToken s = case s of
                "+" -> Just Plus
                "-" -> Just Minus
                "(" -> Just LeftBrace
                ")" -> Just RightBrace
                otherwise -> toNumber s
            where
                toNumber s = if all isDigit s then Just $ Number (read s :: Int) else Nothing
    
tokenize :: String -> Maybe [Token]
tokenize input = sequence $ map asToken $ words input


test1 = (asToken "123") == Just (Number 123)
test2 = (asToken "abc") == Nothing
test3 = (tokenize "1 + 2") == Just [Number 1,Plus,Number 2]
test4 = (tokenize "1 + ( 7 - 2 )") == Just [Number 1,Plus,LeftBrace,Number 7,Minus,Number 2,RightBrace]
test5 = (tokenize "1 + abc") == Nothing

testAll = test1 && test2 && test3 && test4 && test5