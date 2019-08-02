{-
Реализуйте функцию findDigitOrX, использующую функцию findDigit (последнюю реализовывать не нужно). 
findDigitOrX должна находить цифру в строке, а если в строке цифр нет, то она должна возвращать символ 'X'. Используйте конструкцию case.
-}

import Data.Char(isDigit)

findDigit :: [Char] -> Maybe Char
findDigit [] = Nothing
findDigit (c:s) | isDigit c = Just c
                | otherwise = findDigit s

findDigitOrX :: [Char] -> Char
findDigitOrX s = case findDigit s of
                   Just c  -> c
                   Nothing -> 'X'