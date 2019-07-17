{-
Целое число можно представить как список битов со знаком.

Реализуйте функции сложения и умножения для таких целых чисел, считая, что младшие биты идут в начале списка, а старшие — в конце. 
Можно считать, что на вход не будут подаваться числа с ведущими нулями. 
-}

module Bit where

data Bit = Zero | One deriving (Eq, Show)
data Sign = Minus | Plus deriving (Eq, Show)
data Z = Z Sign [Bit] deriving (Eq, Show)

--instance Eq Z where
--    (Z s x) == (Z s' y) = s == s' && x == y

bitToInt :: Bit -> Int
bitToInt Zero = 0
bitToInt One  = 1

intToBit :: Int -> Bit
intToBit 0 = Zero
intToBit 1 = One
intToBit _ = error "Unexpected value!!!"

signToOp :: Sign -> Int -> Int -> Int
signToOp Minus = (-)
signToOp Plus  = (+)

emptyZ = undefined

add :: Z -> Z -> Z
add = undefined

mul :: Z -> Z -> Z
mul = undefined


addHelper xs ys = reverse $ rest':bits'
    where
        (bits', rest') = foldl (\(bits, rest) (x, y) -> ((addBits x y rest):bits, calcRest x y rest)) ([], Zero) zs
        addBits x y r = intToBit $ (bitToInt x + bitToInt y + bitToInt r) `mod` 2
        calcRest x y r = intToBit $ (bitToInt x + bitToInt y + bitToInt r) `div` 2
        zs = zip xs ys

subHelper xs ys = reverse $ rest':bits'
    where
        (bits', rest') = foldl (\(bits, rest) (x, y) -> ((subBits x y rest):bits, calcRest x y rest)) ([], Zero) zs
        subBits x y r = intToBit $ (bitToInt x - bitToInt y - bitToInt r) `mod` 2
        calcRest x y r = intToBit $ (bitToInt x - bitToInt y - bitToInt r) `div` 2
        zs = zip xs ys


-- test001 = (add (Z Plus []) (Z Plus [])) == Z Plus []
-- test002 = (add (Z Plus []) (Z Plus [One])) == Z Plus [One]
-- test003 = (add (Z Plus []) (Z Minus [One])) == Z Minus [One]

-- test011 = (add (Z Plus [Zero, One, One]) (Z Plus [One])) == Z Plus [One, One, One]
-- test012 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One])) == Z Plus [Zero, Zero, Zero, One]
-- test013 = (add (Z Plus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus [Zero, Zero, One, One]

-- test021 = (add (Z Minus [Zero, One, One]) (Z Minus [One])) == Z Minus [One, One, One]
-- test022 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One])) == Z Minus [Zero, Zero, Zero, One]
-- test023 = (add (Z Minus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One, One]

-- test031 = (add (Z Minus [Zero, One, One]) (Z Plus [One])) == Z Minus [One, Zero, One]
-- test032 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One])) == Z Minus [Zero, Zero, One]
-- test033 = (add (Z Minus [Zero, One, One]) (Z Plus [Zero, One, One])) == Z Plus []

-- test041 = (add (Z Plus [Zero, One, One]) (Z Minus [One])) == Z Plus [One, Zero, One]
-- test042 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One])) == Z Plus [Zero, Zero, One]
-- test043 = (add (Z Plus [Zero, One, One]) (Z Minus [Zero, One, One])) == Z Plus []

-- test051 = (add (Z Plus [One]) (Z Minus [One])) == Z Plus []
-- test052 = (add (Z Plus [One]) (Z Minus [One, One])) == Z Minus [Zero, One]
-- test053 = (add (Z Plus [One]) (Z Minus [Zero, One])) == Z Minus [One]
-- test054 = (add (Z Plus [One]) (Z Minus [Zero, Zero, Zero, One])) == Z Minus [One, One, One]
-- test055 = (add (Z Plus [One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, Zero, One]
-- test056 = (add (Z Plus [Zero, One]) (Z Minus [Zero, One, One])) == Z Minus [Zero, Zero, One]
-- test057 = (add (Z Plus [Zero, One]) (Z Minus [Zero, Zero, One])) == Z Minus [Zero, One]
-- test058 = (add (Z Plus [One, Zero, One]) (Z Minus [Zero, One, Zero, One])) == Z Minus [One, Zero, One]


-- test101 = (mul (Z Plus []) (Z Plus [])) == emptyZ
-- test102 = (mul (Z Plus []) (Z Plus [One])) == emptyZ
-- test103 = (mul (Z Plus []) (Z Minus [One])) == emptyZ
-- test104 = (mul (Z Plus [One]) (Z Plus [])) == emptyZ
-- test105 = (mul (Z Minus [One]) (Z Plus [])) == emptyZ

-- test111 = (mul (Z Plus [One]) (Z Plus [One])) == Z Plus [One]
-- test112 = (mul (Z Minus [One]) (Z Plus [One])) == Z Minus [One]
-- test113 = (mul (Z Plus [One]) (Z Minus [One])) == Z Minus [One]
-- test114 = (mul (Z Minus [One]) (Z Minus [One])) == Z Plus [One]

-- test121 = (mul (Z Plus [One]) (Z Plus [Zero, One])) == Z Plus [Zero, One]
-- test122 = (mul (Z Plus [Zero, Zero, One]) (Z Plus [Zero, Zero, One])) == Z Plus [Zero, Zero, Zero, Zero, One]

-- test131 = (mul (Z Plus [One, Zero, One, Zero, One]) (Z Plus [One, One, One])) == Z Plus [One, One, Zero, Zero, One, Zero, Zero, One]


-- testAdd = test001 && test002 && test003 && test011 && test012 && test013 && test021 && test022 && test023 && test031 && test032 && test033 && test041 && test042 && test043 && test051 && test052 && test053 && test054 && test055 && test056 && test057 && test058
-- testMul = test101 && test102 && test103 && test104 && test105 && test111 && test112 && test113 && test114 && test121 && test122 && test131

-- testAll = testAdd && testMul