module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Block1.Test1 (test)
import qualified Block2.Test1 (test)
import qualified Block2.Test2 (test)
import qualified Block3.Test2 (test)

main :: IO ()
main = do
  testBlock1Task1 <- Block1.Test1.test
  testBlock2Task1 <- Block2.Test1.test
  testBlock2Task2 <- Block2.Test2.test
  testBlock3Task2 <- Block3.Test2.test

  defaultMain $ testGroup "HW2" [testBlock1Task1, testBlock2Task1, testBlock2Task2, testBlock3Task2]