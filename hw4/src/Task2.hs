module Task2
  ( seqIntegrate
  , parIntegrate
  , Range
  ) where

import System.Random               ( mkStdGen
                                   , randomR
                                   , random
                                   )
import Control.Parallel.Strategies ( runEval
                                   , rpar
                                   , parMap
                                   )
import Data.List                   ( unfoldr
                                   )

type Range = (Double, Double)

mcCount :: Int
mcCount = 1000000

mcSum :: Range -> (Double -> Double) -> Int -> Int -> Double
mcSum (a, b) f cnt seed =
  let gen = mkStdGen seed
      rs  = take cnt . unfoldr (Just . randomR (a, b))
  in sum $ map f $ rs gen

seqIntegrate :: Range -> (Double -> Double) -> Double
seqIntegrate rng@(a, b) f = 
  let s = mcSum rng f mcCount 1337
  in (b - a) / (fromIntegral mcCount) * s

parIntegrate :: Range -> (Double -> Double) -> Int -> Double
parIntegrate rng@(a, b) f pars = runEval $ do
  let gen = mkStdGen 100500
  let seeds = (take pars . unfoldr (Just . random)) gen
  let batch = mcCount `div` pars
  let rs = parMap rpar (mcSum rng f batch) seeds
  return $ (b - a) / (fromIntegral (batch * pars)) * (sum rs)