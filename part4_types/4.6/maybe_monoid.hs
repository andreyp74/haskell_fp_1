{-
Реализуйте представителя класса типов Monoid для Maybe' a так, чтобы mempty не был равен Maybe' Nothing. 
Нельзя накладывать никаких дополнительных ограничений на тип a, кроме указанных в условии.
-}

module MaybeMonoid where

newtype Maybe' a = Maybe' { getMaybe :: Maybe a }
    deriving (Eq,Show)

instance Semigroup a => Semigroup (Maybe' a) where
    Maybe' (Just x) <> Maybe' (Just y) = Maybe' $ Just ( x <> y)
    Maybe' (Just x) <> Maybe' Nothing  = Maybe' (Just x) 
    Maybe' Nothing  <> other = other

instance Monoid a => Monoid (Maybe' a) where
    mempty = Maybe' (Just mempty)
   


-- instance Monoid a => Monoid (Maybe' a) where
--     mempty = Maybe' (Just mempty)
--     Maybe' (Just x) `mappend` Maybe' (Just y) = Maybe' $ Just ( x `mappend` y)
--     Maybe' (Just x) `mappend` Maybe' Nothing  = Maybe' (Just x) 
--     Maybe' Nothing  `mappend` other = other

test0 = if (mempty :: Maybe' [Int]) == Maybe' Nothing then "failed" else "passed"