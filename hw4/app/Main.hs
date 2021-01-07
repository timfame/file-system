module Main where

import Task8 (Config (..), simulateCOVID)

main :: IO ()
main = do
  simulateCOVID $ Config 0.3 3 2 5
  
