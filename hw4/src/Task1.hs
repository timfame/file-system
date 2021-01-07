{-# LANGUAGE BangPatterns #-}

module Task1 
  ( Point (..)
  , plus
  , minus
  , scalarProduct
  , crossProduct
  , perimeter
  , perimeterSlow
  , doubleArea
  , doubleAreaSlow
  ) where

data Point 
  = Point 
  { x :: !Int
  , y :: !Int
  }
  deriving (Show)

plus :: Point -> Point -> Point
plus p1 p2 = Point ((x p1) + (x p2)) ((y p1) + (y p2))  

minus :: Point -> Point -> Point
minus p1 p2 = Point ((x p1) - (x p2)) ((y p1) - (y p2))  

scalarProduct :: Point -> Point -> Int
scalarProduct p1 p2 = (x p1) * (x p2) + (y p1) * (y p2) 

crossProduct  :: Point -> Point -> Int
crossProduct p1 p2  = (x p1) * (y p2) - (x p2) * (y p1)

calcDistance  :: Point -> Point -> Double
calcDistance p1 p2 = sqrt $ fromIntegral $ scalarProduct diff diff
  where
    diff = minus p1 p2

perimeter :: [Point] -> Double
perimeter []     = 0
perimeter (p:ps) = go 0 (p:ps)
  where
    go  _   []               = 0
    go !acc [lp]             = acc + (calcDistance lp p)
    go !acc (p1:rest@(p2:_)) = go (acc + (calcDistance p1 p2)) rest

perimeterSlow :: [Point] -> Double
perimeterSlow []     = 0
perimeterSlow (p:ps) = go 0 (p:ps)
  where
    go _   []               = 0
    go acc [lp]             = acc + (calcDistance lp p)
    go acc (p1:rest@(p2:_)) = go (acc + (calcDistance p1 p2)) rest

doubleArea :: [Point] -> Double
doubleArea []     = 0
doubleArea (p:ps) = abs $ fromIntegral $ go 0 (p:ps)
  where
    go  _   []               = 0
    go !acc [lp]             = acc + (crossProduct lp p)
    go !acc (p1:rest@(p2:_)) = go (acc + (crossProduct p1 p2)) rest

doubleAreaSlow :: [Point] -> Double
doubleAreaSlow []     = 0
doubleAreaSlow (p:ps) = abs $ fromIntegral $ go 0 (p:ps)
  where
    go _   []               = 0
    go acc [lp]             = acc + (crossProduct lp p)
    go acc (p1:rest@(p2:_)) = go (acc + (crossProduct p1 p2)) rest