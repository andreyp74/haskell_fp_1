{-
Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности

\[ a_0 = 1; a_1 = 2 ; a_2 = 3; a_{k+3} = a_{k+2} + a_{k+1} - 2 a_{k}. \]
Попытайтесь найти эффективное решение.

GHCi> seqA 301
1276538859311178639666612897162414
-}

module SeqA where

seqA :: Integer -> Integer
seqA 0 = 1
seqA 1 = 2
seqA 2 = 3
seqA n = 
    let 
        helper :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
        helper k k1 k2 i n = 
            let 
                k3 = k2 + k1 - 2*k 
            in if i == n then k3 else helper k1 k2 k3 (i + 1) n
    in helper 1 2 3 3 n
