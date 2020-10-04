{-# LANGUAGE BlockArguments #-}

module Block1.Test2
  ( test
  ) where

import Block1.Task2
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Nat" $ do
    describe "Plus" $ do
      it "Z + Z                = Z" $ (Z + Z) `shouldBe` Z
      it "Z + (S Z)            = (S Z)" $ (Z + (S Z)) `shouldBe` (S Z)
      it "Z + (S (S Z))        = (S (S Z))" $ (Z + (S (S Z))) `shouldBe` (S (S Z))
      it "(S Z) + Z            = (S Z)" $ ((S Z) + Z) `shouldBe` (S Z)
      it "(S (S Z)) + Z        = (S (S Z))" $ ((S (S Z)) + Z) `shouldBe` (S (S Z))
      it "(S Z) + (S Z)        = (S (S Z))" $ ((S Z) + (S Z)) `shouldBe` (S (S Z))
      it "9 :: Nat + 2 :: Nat  = 11 :: Nat" $ ((9 :: Nat) + (2 :: Nat)) `shouldBe` (11 :: Nat)
      it "5 :: Nat + 10 :: Nat = 15 :: Nat" $ ((5 :: Nat) + (10 :: Nat)) `shouldBe` (15 :: Nat)

    describe "Multiply" $ do
      it "Z * Z                 = Z" $ (Z * Z) `shouldBe` Z
      it "Z * (S Z)             = Z" $ (Z * (S Z)) `shouldBe` Z
      it "Z * (S (S Z))         = Z" $ (Z * (S (S Z))) `shouldBe` Z
      it "(S Z) * Z             = Z" $ ((S Z) * Z) `shouldBe` Z
      it "(S (S Z)) * Z         = Z" $ ((S (S Z)) * Z) `shouldBe` Z
      it "(S Z) * (S Z)         = (S Z)" $ ((S Z) * (S Z)) `shouldBe` (S Z)
      it "(S (S Z)) * (S (S Z)) = (S (S (S (S Z))))" $ (S (S Z)) * (S (S Z)) `shouldBe` (S (S (S (S Z))))
      it "9 :: Nat * 2 :: Nat   = 18 :: Nat" $ ((9 :: Nat) * (2 :: Nat)) `shouldBe` (18 :: Nat)
      it "5 :: Nat * 10 :: Nat  = 50 :: Nat" $ ((5 :: Nat) * (10 :: Nat)) `shouldBe` (50 :: Nat)

    describe "Minus" $ do
      it "Z - Z                = Z" $ (Z - Z) `shouldBe` Z
      it "(S Z) - Z            = (S Z)" $ ((S Z) - Z) `shouldBe` (S Z)
      it "(S (S Z)) - Z        = (S (S Z))" $ ((S (S Z)) - Z) `shouldBe` (S (S Z))
      it "(S Z) - (S Z)        = Z" $ ((S Z) - (S Z)) `shouldBe` Z
      it "9 :: Nat - 2 :: Nat  = 7 :: Nat" $ ((9 :: Nat) - (2 :: Nat)) `shouldBe` (7 :: Nat)
      it "10 :: Nat - 5 :: Nat = 5 :: Nat" $ ((10 :: Nat) - (5 :: Nat)) `shouldBe` (5 :: Nat)

    describe "Equality" $ do
      it "Z           == Z" $ (Z == Z) `shouldBe` True
      it "Z           == (S Z)" $ (Z == (S Z)) `shouldBe` False
      it "(S Z)       == Z" $ ((S Z) == Z) `shouldBe` False
      it "(S Z)       == (S Z)" $ ((S Z) == (S Z)) `shouldBe` True
      it "(7 :: Nat)  == (7 ::Nat)" $ ((7 :: Nat) == (7 :: Nat)) `shouldBe` True
      it "(7 :: Nat)  /= (7 ::Nat)" $ ((7 :: Nat) /= (7 :: Nat)) `shouldBe` False
      it "(17 :: Nat) == (7 ::Nat)" $ ((17 :: Nat) == (7 :: Nat)) `shouldBe` False
      it "(17 :: Nat) /= (7 ::Nat)" $ ((17 :: Nat) /= (7 :: Nat)) `shouldBe` True

    describe "Order" $ do
      it "Z           <= Z" $ (Z <= Z) `shouldBe` True
      it "Z           <= (S Z)" $ (Z <= (S Z)) `shouldBe` True
      it "(S Z)       <= Z" $ ((S Z) <= Z) `shouldBe` False
      it "(S Z)       >= (S Z)" $ ((S Z) >= (S Z)) `shouldBe` True
      it "(7 :: Nat)   < (7 ::Nat)" $ ((7 :: Nat) < (7 :: Nat)) `shouldBe` False
      it "(7 :: Nat)  <= (7 ::Nat)" $ ((7 :: Nat) <= (7 :: Nat)) `shouldBe` True
      it "(17 :: Nat)  > (7 ::Nat)" $ ((17 :: Nat) > (7 :: Nat)) `shouldBe` True
      it "(17 :: Nat) <= (7 ::Nat)" $ ((17 :: Nat) <= (7 :: Nat)) `shouldBe` False

  