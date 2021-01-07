module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Test3 (test) 

main :: IO ()
main = do
  testTask3 <- Test3.test
  
  defaultMain $ testGroup "HW3" [testTask3]