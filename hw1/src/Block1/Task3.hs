{-# LANGUAGE InstanceSigs #-}

module Block1.Task3 where

import Prelude hiding (head)
import Data.List.NonEmpty hiding (insert, fromList)

data Tree a = Leaf | Node (NonEmpty a) (Tree a) (Tree a) deriving Show

isEmpty :: Tree a -> Bool
isEmpty Leaf = True
isEmpty _ = False

size :: Tree a -> Int
size Leaf = 1
size (Node value left right) = 1 + size left + size right

find :: (Ord a) => Tree a -> a -> Tree a
find Leaf _ = Leaf
find (Node value left right) key | (head value) == key = Node value left right
                                 | (head value) < key  = find right key
                                 | otherwise           = find left key

insert :: (Ord a) => Tree a -> a -> Tree a
insert Leaf key = Node (key :| []) Leaf Leaf
insert (Node value left right) key | (head value) == key = Node (key <| value) left right
                                   | (head value) < key  = Node value left (insert right key)
                                   | otherwise           = Node value (insert left key) right

fromList :: (Ord a) => [a] -> Tree a
fromList [] = Leaf
fromList (x:xs) = insert (fromList xs) x

instance Foldable Tree where
    foldr :: (a -> b -> b) -> b -> Tree a -> b
    foldr _ v Leaf = v
    foldr f v (Node value left right) = foldr f (foldr f (foldr f v left) value) right

    foldMap :: Monoid m => (a -> m) -> Tree a -> m
    foldMap _ Leaf = mempty
    foldMap m (Node value left right) = foldMap m left <> foldMap m value <> foldMap m right