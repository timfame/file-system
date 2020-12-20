{-# LANGUAGE BlockArguments #-}

module Block2.Test1
  ( test
  ) where

import Block2.Task1 (Expr(..), ArithmeticError(..), eval)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Block2 - Expression" $ do
    describe "Const" $ do
      it "Const (-2)" $ eval (Const (-2)) `shouldBe` Right (-2)
      it "Const 1000" $ eval (Const 1000) `shouldBe` Right 1000
    describe "Add" $ do
      it "Add (Const 2) (Const 5)"     $ eval (Add (Const 2) (Const 5))     `shouldBe` Right 7
      it "Add (Const 2) (Const (-2))"  $ eval (Add (Const 2) (Const (-2)))  `shouldBe` Right 0
      it "Add (Const 5) (Const (-10))" $ eval (Add (Const 5) (Const (-10))) `shouldBe` Right (-5)
    describe "Sub" $ do
      it "Sub (Const 2) (Const 5)"     $ eval (Sub (Const 2) (Const 5))     `shouldBe` Right (-3)
      it "Sub (Const 2) (Const (-2))"  $ eval (Sub (Const 2) (Const (-2)))  `shouldBe` Right 4
      it "Sub (Const 5) (Const (-10))" $ eval (Sub (Const 5) (Const (-10))) `shouldBe` Right 15
    describe "Mul" $ do
      it "Mul (Const 2) (Const 5)"     $ eval (Mul (Const 2) (Const 5))     `shouldBe` Right 10
      it "Mul (Const 2) (Const (-2))"  $ eval (Mul (Const 2) (Const (-2)))  `shouldBe` Right (-4)
      it "Mul (Const 5) (Const (-10))" $ eval (Mul (Const 5) (Const (-10))) `shouldBe` Right (-50)
      it "Mul (Const 10000) (Const 0)" $ eval (Mul (Const 10000) (Const 0)) `shouldBe` Right 0
    describe "Div" $ do
      it "Div (Const 2) (Const 5)"     $ eval (Div (Const 2) (Const 5))     `shouldBe` Right 0
      it "Div (Const 2) (Const (-2))"  $ eval (Div (Const 2) (Const (-2)))  `shouldBe` Right (-1)
      it "Div (Const 6) (Const (-2))"  $ eval (Div (Const 6) (Const (-2)))  `shouldBe` Right (-3)
      it "Div (Const 10000) (Const 0)" $ eval (Div (Const 10000) (Const 0)) `shouldBe` Left DivisionByZero
    describe "Pow" $ do
      it "Pow (Const 2) (Const 5)"     $ eval (Pow (Const 2) (Const 5))     `shouldBe` Right 32
      it "Pow (Const 2) (Const (-2))"  $ eval (Pow (Const 2) (Const (-2)))  `shouldBe` Left NegatePower
      it "Pow (Const 10000) (Const 0)" $ eval (Pow (Const 10000) (Const 0)) `shouldBe` Right 1
    describe "Complicated success expressions" $ do
      it "(Mul (Add (Const 2) (Const 5)) (Div (Const 5) (Pow (Const 100) (Const 0))))" $ eval (Mul (Add (Const 2) (Const 5)) (Div (Const 5) (Pow (Const 100) (Const 0)))) `shouldBe` Right 35
    describe "Complicated failed expressions" $ do
      it "(Mul (Add (Const 2) (Const 5)) (Div (Const 5) (Pow (Const 100) (Const (-1)))))" $ eval (Mul (Add (Const 2) (Const 5)) (Div (Const 5) (Pow (Const 100) (Const (-1))))) `shouldBe` Left NegatePower
      it "(Mul (Add (Const 2) (Const 5)) (Div (Const 5) (Sub (Const 100) (Pow (Const 100) (Const 1))))" $ eval (Mul (Add (Const 2) (Const 5)) (Div (Const 5) (Sub (Const 100) (Pow (Const 100) (Const 1))))) `shouldBe` Left DivisionByZero