module Block1.Task3
    ( NonEmpty
    ) where


data NonEmpty a
    = a :| [a]
    deriving (Show)


instance Functor NonEmpty where
    fmap f (x :| xs) = (f x) :| (fmap f xs)


instance Applicative NonEmpty where
    pure x = x :| []

    (l :| ls) <*> (r :| rs) = (l r) :| mappend (l <$> rs) (mappend (ls <*> [r]) (ls <*> rs))


instance Foldable NonEmpty where
    foldr f acc (x :| xs) = f x (foldr f acc xs)


instance Traversable NonEmpty where
    traverse f (x :| xs) = (fmap (:|) (f x)) <*> (traverse f xs)


instance Monad NonEmpty where
    return x        = x :| []
    (x :| xs) >>= k = head l :| (tail l ++ (xs >>= convertToList . k))
        where l = convertToList $ k x


convertToList :: NonEmpty a -> [a]
convertToList (x :| xs) = x : xs