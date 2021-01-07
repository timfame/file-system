module Utils.Task2
  ( intFunc
  , intRange
  , parsToBench
  ) where

import Task2 (Range)

intFunc :: Double -> Double
intFunc x = 1 / tan(x * x) - cos(x)

intRange :: Range
intRange = (0.5, 1)

parsToBench :: [Int]
parsToBench = [10, 100, 1000, 10000, 100000]
