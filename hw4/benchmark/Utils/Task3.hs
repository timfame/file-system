module Utils.Task3
  ( seqSimulation
  , concurrentSimulation
  , keysSize
  ) where

import Task3              ( ConcurrentHashTable
                          , newCHT
                          , getCHT
                          , putCHT
                          , sizeCHT)
import Control.Concurrent ( forkIO
                          , newEmptyMVar
                          , putMVar
                          , takeMVar
                          )

keysSize :: Int
keysSize = 2000

seqSimulation :: IO ()
seqSimulation = do
  cht <- newCHT
  mapM_ 
    ( \key -> do 
      let val = "value#" ++ show key
      _ <- getCHT  key cht
      putCHT  key val cht
      _ <- getCHT  key cht
      _ <- sizeCHT cht

      let val10 = "value#" ++ show (key * 10)
      _ <- getCHT  (key * 10) cht
      putCHT  (key * 10) val10 cht
      _ <- getCHT  (key * 10) cht
      _ <- sizeCHT cht
      return ()
    ) 
    ([1 .. keysSize] :: [Int])

simpleSimulation :: ConcurrentHashTable Int String -> [Int] -> IO ()
simpleSimulation cht keys = do
  mapM_ 
    ( \key -> do 
      let value = "value#" ++ show key
      _ <- getCHT  key cht
      putCHT  key value cht
      _ <- getCHT  key cht
      _ <- sizeCHT cht
      return ()
    ) 
    keys
  return ()

concurrentSimulation :: IO ()
concurrentSimulation = do
  cht <- newCHT

  let part1 = keysSize `div` 2

  let keys1 = [1 .. part1]            :: [Int]
  let keys2 = [part1 + 1 .. keysSize] :: [Int]

  tm1 <- newEmptyMVar
  tm2 <- newEmptyMVar
  tm3 <- newEmptyMVar
  tm4 <- newEmptyMVar

  _tid1 <- forkIO $ do
    simpleSimulation cht keys1
    putMVar tm1 True
  _tid2 <- forkIO $ do
    simpleSimulation cht $ map (* 10) keys1
    putMVar tm2 True
  _tid3 <- forkIO $ do
    simpleSimulation cht keys2
    putMVar tm3 True
  _tid4 <- forkIO $ do
    simpleSimulation cht $ map (* 10) keys2
    putMVar tm4 True

  _ <- takeMVar tm1
  _ <- takeMVar tm2
  _ <- takeMVar tm3
  _ <- takeMVar tm4

  return ()
  