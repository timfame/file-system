module Block1.Task2
    ( Tree
    ) where

data Tree a
    = Branch (Tree a) (Tree a)
    | Leaf a
    deriving (Show)

instance Functor Tree where
    fmap f (Leaf v)     = Leaf (f v)
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
    pure = Leaf
    (Leaf l)       <*> (Leaf r)       = Leaf (l r)
    (Branch l1 r1) <*> (Branch l2 r2) = Branch (l1 <*> l2)       (r1 <*> r2)
    (Leaf l)       <*> (Branch l2 r2) = Branch ((Leaf l) <*> l2) ((Leaf l) <*> r2)  
    (Branch l1 r1) <*> (Leaf r)       = Branch (l1 <*> (Leaf r)) (r1 <*> (Leaf r))

instance Foldable Tree where
    foldr f acc (Leaf v)     = f v acc
    foldr f acc (Branch l r) = foldr f (foldr f acc r) l 


instance Traversable Tree where
    traverse f (Leaf v)     = Leaf <$> f v
    traverse f (Branch l r) = (fmap Branch (traverse f l)) <*> (traverse f r)