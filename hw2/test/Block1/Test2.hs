{-# LANGUAGE BlockArguments #-}

module Block1.Test2
  ( test
  ) where

import Block1.Task2 (Tree)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Tree instances" $ do
    describe "Functor default" $ do
      it "fmap (+2) Branch (Leaf 5) (Leaf 10)" $ fmap (+2) Branch (Leaf 5) (Leaf 10) `shouldBe` Branch (Leaf 7) (Leaf 12)
      