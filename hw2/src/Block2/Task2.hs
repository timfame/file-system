module Block2.Task2
    ( moving
    ) where

import Control.Monad.State

calcRanges :: Fractional a => [a] -> State (Int, a, Int, [a]) [a]
calcRanges [] = return []
calcRanges (x:xs) = do
    (i, prevSum, size, y) <- get
    let (currentSum, nextY, rangeSize) = if i >= size then (prevSum + x - head y, tail y, size) else (prevSum + x, y, i + 1)
    put(i + 1, currentSum, size, nextY)
    tailRes <- calcRanges xs
    return $ (currentSum / (fromIntegral rangeSize)):tailRes


moving :: Fractional a => Int -> [a] -> [a]
moving sz l = evalState (calcRanges l) (0, 0, sz, l)