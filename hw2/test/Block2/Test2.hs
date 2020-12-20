{-# LANGUAGE BlockArguments #-}

module Block2.Test2
  ( test
  ) where

import Block2.Task2 (moving)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Block2 - Simple Moving Average" $ do
    describe "common tests" $ do
      it "moving 4 [1, 5, 3, 8, 7, 9, 6]" $ moving 4 ([1, 5, 3, 8, 7, 9, 6] :: [Double]) `shouldBe` [1.0, 3.0, 3.0, 4.25, 5.75, 6.75, 7.5]
      it "moving 2 [1, 5, 3, 8, 7, 9, 6]" $ moving 2 ([1, 5, 3, 8, 7, 9, 6] :: [Double]) `shouldBe` [1.0, 3.0, 4.0, 5.5, 7.5, 8.0, 7.5]
      it "moving 1 [1, 2, 3, 4]" $ moving 1 ([1, 2, 3, 4] :: [Double]) `shouldBe` [1.0, 2.0, 3.0, 4.0]
      it "moving 2 [1, 2, 3, 4]" $ moving 2 ([1, 2, 3, 4] :: [Double]) `shouldBe` [1.0, 1.5, 2.5, 3.5]
      it "moving 2 [1, 2]" $ moving 2 ([10, 11] :: [Double]) `shouldBe` [10, 10.5]
      it "moving 5 [3, 3, 3, 3, 3]" $ moving 5 ([3, 3, 3, 3, 3] :: [Double]) `shouldBe` [3, 3, 3, 3, 3]