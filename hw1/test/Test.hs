module Main where

import Test.Tasty (defaultMain, testGroup)

import qualified Block1.Test1 (test)
import qualified Block1.Test2 (test)

main :: IO ()
main = do
  testBlock1Task1 <- Block1.Test1.test
  testBlock1Task2 <- Block1.Test2.test

  defaultMain $ testGroup "Block1" [testBlock1Task1, testBlock1Task2]