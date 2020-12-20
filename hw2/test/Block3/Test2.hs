{-# LANGUAGE BlockArguments #-}

module Block3.Test2
  ( test
  ) where

import Block3.Task1 (Parser(..))
import Block3.Task2 (ok, eof, satisfy, element, stream)
import Test.Tasty (TestTree)
import Test.Tasty.Hspec (describe, it, shouldBe, testSpec)

test :: IO TestTree
test =
  testSpec "Block3 - Parsers" $ do
    describe "ok" $ do
      it "runParser ok \"abc\"" $ runParser ok "abc" `shouldBe` Just ((), "abc")
      it "runParser ok \"   \"" $ runParser ok "   " `shouldBe` Just ((), "   ")
      it "runParser ok \"\"" $ runParser ok "" `shouldBe` Just ((), "")
    describe "eof" $ do
      it "runParser eof \"\"" $ runParser eof "" `shouldBe` Just ((), "")
      it "runParser eof \"_\"" $ runParser eof "_" `shouldBe` Nothing
      it "runParser eof \"abc\"" $ runParser eof "abc" `shouldBe` Nothing
    describe "satisfy" $ do
      it "runParser (satisfy (\\c -> c == 'a')) \"abc\"" $ runParser (satisfy (\c -> c == 'a')) "abc" `shouldBe` Just ('a', "bc")
      it "runParser (satisfy (\\c -> c == 'a')) \"bbc\"" $ runParser (satisfy (\c -> c == 'a')) "bbc" `shouldBe` Nothing
    describe "element" $ do
      it "runParser (element \"abc\") [\"abc\", \"def\", \"ghi\"]" $ runParser (element "abc") ["abc", "def", "ghi"] `shouldBe` Just ("abc", ["def", "ghi"])
      it "runParser (element \"abc\") [\"bbc\", \"def\", \"ghi\"]" $ runParser (element "abc") ["bbc", "def", "ghi"] `shouldBe` Nothing
    describe "stream" $ do
      it "runParser (stream [\"ab\", \"cd\"]) [\"ab\", \"cd\", \"fe\"]" $ runParser (stream ["ab", "cd"]) ["ab", "cd", "fe"] `shouldBe` Just (["ab", "cd"], ["fe"])
      it "runParser (stream [1, 2]) [1, 2, 3, 4] ([Int] cast needed)" $ runParser (stream ([1, 2] :: [Int])) ([1, 2, 3, 4] :: [Int]) `shouldBe` Just ([1, 2], [3, 4])