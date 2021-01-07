{-# LANGUAGE BlockArguments #-}

module Test3
  ( test
  ) where

import Task3              ( newCHT
                          , getCHT
                          , putCHT
                          , sizeCHT
                          )
import Test.Tasty         ( TestTree)
import Test.Tasty.Hspec   ( describe
                          , it
                          , shouldBe
                          , testSpec
                          )
import Control.Concurrent ( forkIO
                          , newEmptyMVar
                          , putMVar
                          , takeMVar
                          )

test :: IO TestTree
test =
  testSpec "ConcurrentHashTable" $ do
    describe "Simple operations" $ do

      it "Empty table size" $ do 
        cht <- newCHT
        sz  <- sizeCHT cht 
        sz `shouldBe` 0

      it "One element with update" $ do
        cht <- newCHT
        let key = 10 :: Int
        putCHT key "first" cht

        valJ <- getCHT key cht
        valN <- getCHT (key + 1) cht
        sz   <- sizeCHT cht
        valJ `shouldBe` (Just "first")
        valN `shouldBe` Nothing
        sz   `shouldBe` 1

        putCHT key "second" cht
        newVal <- getCHT key cht
        newSz  <- sizeCHT cht
        newVal `shouldBe` (Just "second")
        newSz  `shouldBe` 1

      it "Multiple puts" $ do
        cht <- newCHT
        let keys = [10..15] :: [Int]

        mapM_
          ( \key -> do
            putCHT key ("test#" ++ show key) cht
            valJ <- getCHT key cht
            valN <- getCHT (key + 1) cht
            sz <- sizeCHT cht
            valJ `shouldBe` (Just ("test#" ++ show key))
            valN `shouldBe` Nothing
            sz `shouldBe` (key - 9)
          )
          keys

      it "Concurrent usage" $ do
        cht <- newCHT
        let keys = [10..1000] :: [Int]

        tm1 <- newEmptyMVar
        tm2 <- newEmptyMVar

        _tid1 <- forkIO $ do
          mapM_ 
            ( \key -> do 
              putCHT key ("test1#" ++ show key) cht
              val <- getCHT key cht
              sz  <- sizeCHT cht
              val             `shouldBe` (Just ("test1#" ++ show key))
              (sz >= key - 9) `shouldBe` True
            ) 
            keys
          putMVar tm1 True

        _tid2 <- forkIO $ do
          mapM_ 
            ( \key -> do 
              putCHT key ("test2#" ++ show key) cht
              val <- getCHT key cht
              sz  <- sizeCHT cht
              val                       `shouldBe` (Just ("test2#" ++ show key))
              (sz >= (key `div` 2) - 9) `shouldBe` True
            ) 
            $ map (* 2) keys
          putMVar tm2 True

        _ <- takeMVar tm1
        _ <- takeMVar tm2

        sz <- sizeCHT cht
        sz `shouldBe` 991 + 500
