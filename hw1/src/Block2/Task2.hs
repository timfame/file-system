module Block2.Task2 where

import Data.List.NonEmpty

splitFold :: Eq a => a -> a -> NonEmpty [a] -> NonEmpty [a]
splitFold sep cur (x :| xs) | cur == sep = [] :| (x : xs)
                            | otherwise  = (cur : x) :| xs

splitOn :: Eq a => a -> [a] -> NonEmpty [a]
splitOn sep list = foldr (splitFold sep) ([] :| []) list