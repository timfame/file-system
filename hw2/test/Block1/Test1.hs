{-# LANGUAGE BlockArguments #-}

module Block1.Test1
  ( test
  ) where

import Block1.Task1 (stringSum)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Block1 - stringSum" $ do
    describe "Just result" $ do
      it "2 10 3" $ stringSum "2 10 3" `shouldBe` Just 15
      it "10 20 " $ stringSum "10 20 " `shouldBe` Just 30
      it "111"    $ stringSum "111"    `shouldBe` Just 111
      it "-1"     $ stringSum "-1"     `shouldBe` Just (-1)
      it "10 -10" $ stringSum "10 -10" `shouldBe` Just 0
      it "-5 -10" $ stringSum "-5 -10" `shouldBe` Just (-15)
      it "10 -99" $ stringSum "10 -99" `shouldBe` Just (-89)
    describe "Nothing result" $ do
      it "2 10 s" $ stringSum "2 10 s" `shouldBe` Nothing
      it "! !"    $ stringSum "! !"    `shouldBe` Nothing
      it "2_10_3" $ stringSum "2_10_3" `shouldBe` Nothing