{-
Реализуйте представителя класса типов Monoid для типа Xor, в котором mappend выполняет операцию xor.
-}

module XorMonoid where

import Data.Monoid

newtype Xor = Xor { getXor :: Bool }
    deriving (Eq,Show)

instance Semigroup Xor where
    Xor x <> Xor y = Xor (x /= y)

instance Monoid Xor where
    mempty = Xor False

-- instance Monoid Xor where
--     mempty = Xor False
--     Xor x `mappend` Xor y = Xor (x /= y)
    