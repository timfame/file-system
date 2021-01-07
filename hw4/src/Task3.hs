module Task3
  ( ConcurrentHashTable
  , newCHT
  , getBucket
  , getCHT
  , putCHT
  , sizeCHT
  ) where

import           Control.Concurrent.STM      ( STM
                                             , atomically
                                             )
import           Control.Concurrent.STM.TVar ( TVar
                                             , newTVar
                                             , readTVar
                                             , writeTVar 
                                             , modifyTVar
                                             )
import           Data.Hashable               ( Hashable
                                             , hash
                                             )
import qualified Data.Vector                 as V

type Bucket k v = TVar (V.Vector (k, v))

data ConcurrentHashTable k v
  = ConcurrentHashTable
  { buckets :: (TVar (V.Vector (Bucket k v)))
  , size    :: (TVar Int)
  }

newCHT :: IO (ConcurrentHashTable k v)
newCHT = atomically $ do
  vs <- V.replicateM 32 (newTVar V.empty)
  bs <- newTVar vs
  sz <- newTVar 0
  return $ ConcurrentHashTable bs sz

getBucket :: Hashable k => k -> ConcurrentHashTable k v -> STM (Bucket k v)
getBucket key cht = do
  bs <- readTVar $ buckets cht
  let index = (hash key) `mod` (V.length bs)
  return $ bs V.! index

ensureSize :: Hashable k => ConcurrentHashTable k v -> STM ()
ensureSize cht = do
  sz <- readTVar $ size cht
  bs <- readTVar $ buckets cht
  if ((V.length bs) * 3 `div` 4 < sz + 1)
    then do
      let capacity = (V.length bs) * 2
      newVs <- V.replicateM capacity (newTVar V.empty)

      V.mapM_
        ( \buck -> do
          vs <- readTVar buck
          V.mapM_
            ( \(key, value) -> do
              let index = (hash key) `mod` capacity
              modifyTVar (newVs V.! index) (\vec -> V.snoc vec (key, value))
              return ()
            )
            vs
        )
        bs

      writeTVar (buckets cht) newVs
    else return ()

getCHT :: (Hashable k, Eq k) => k -> ConcurrentHashTable k v -> IO (Maybe v)
getCHT key cht = atomically $ do
  bucket <- getBucket key cht
  vs <- readTVar bucket
  let found = V.find (\val -> (fst val) == key) vs
  return $ fmap snd found

putCHT :: (Hashable k, Eq k) => k -> v -> ConcurrentHashTable k v -> IO ()
putCHT key value cht = atomically $ do
  ensureSize cht
  bucket <- getBucket key cht
  vs <- readTVar bucket
  case (V.findIndex (\val -> (fst val) == key) vs) of
    Nothing    -> do modifyTVar (size cht) succ
                     modifyTVar bucket (\vec -> V.snoc vec (key, value))
    (Just ind) -> modifyTVar bucket (\vec -> vec V.// [(ind, (key, value))])

sizeCHT :: ConcurrentHashTable k v -> IO Int
sizeCHT cht = atomically $ readTVar $ size cht
