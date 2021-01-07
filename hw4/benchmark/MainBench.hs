module Main where

import Criterion.Main ( defaultMain
                      , bgroup
                      , bench
                      , nf
                      , nfIO
                      )
-- import Task1          ( perimeter
--                       , perimeterSlow
--                       , doubleArea
--                       , doubleAreaSlow
--                       )
import Task2          ( seqIntegrate
                      , parIntegrate
                      )
-- import Utils.Task1    ( generatePoints)
import Utils.Task2    ( intFunc
                      , intRange
                      , parsToBench
                      )
import Utils.Task3    ( seqSimulation
                      , concurrentSimulation
                      , keysSize
                      )

main :: IO ()
main = do
  -- points <- generatePoints 10000000
  defaultMain 
    [ 
    --   bgroup "Task1 - Perimeter"
    --     [ bench "Fast" $ whnf perimeter points
    --     , bench "Slow" $ whnf perimeterSlow points
    --     ]
    -- , bgroup "Task1 - DoubleArea"
    --     [ bench "Fast" $ whnf doubleArea points
    --     , bench "Slow" $ whnf doubleAreaSlow points
    --     ]
     bgroup "Task2 - Integrate"
      $ [bench "Sequential" $ nf (seqIntegrate intRange) intFunc]
        ++ ( map
               ( \x -> 
                   bench 
                     ("Parallel " ++ show x)
                     (nf (parIntegrate intRange intFunc) x)
               )
               parsToBench
           )
    , bgroup "Task3 - HashMap"
        [ bench ("Sequential: 8 * " ++ show keysSize ++ " operations") $ nfIO seqSimulation
        , bench ("Concurrent: 8 * " ++ show keysSize ++ " operations with 4 threads") $ nfIO concurrentSimulation
        ]
    ]
