{-
Реализуйте представителя класса типов Monoid для Maybe' a так, чтобы mempty не был равен Maybe' Nothing. 
Нельзя накладывать никаких дополнительных ограничений на тип a, кроме указанных в условии.
-}

module MaybeMonoid where

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Semigroup a => Semigroup (Maybe' a) where
    Maybe' (Just x) <> Maybe' (Just y) = Maybe' $ Just (x <> y)
    Maybe' Nothing <> _ = Maybe' Nothing
    _ <> Maybe' Nothing = Maybe' Nothing

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
   
-- instance Monoid a => Monoid (Maybe' a) where
--     mempty = Maybe' (Just mempty)
--     Maybe' (Just x) `mappend` Maybe' (Just y) = Maybe' $ Just (x `mappend` y)
--     Maybe' Nothing `mappend` _ = Maybe' Nothing
--     _ `mappend` Maybe' Nothing = Maybe' Nothing

test0 = if (mempty :: Maybe' [Int]) == Maybe' Nothing then "failed" else "passed"