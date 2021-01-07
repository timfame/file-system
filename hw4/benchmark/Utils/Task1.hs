module Utils.Task1
  ( generatePoints
  ) where

import Task1         ( Point (..))
import System.Random ( randomRIO)
import Control.Monad ( replicateM)

generatePoints :: Int -> IO [Point]
generatePoints n = replicateM n $ getRandomPoint

getRandomPoint :: IO Point
getRandomPoint = do
  newX <- randomRIO (-10000000, 10000000)
  newY <- randomRIO (-10000000, 10000000)
  return $ Point newX newY