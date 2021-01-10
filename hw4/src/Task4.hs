{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Task4
  ( Number (..)
  , HScript (..)
  , Convertable (..)
  , runLog2
  , runMainFunc
  , log2
  , mainFunc
  ) where

import Control.Monad.ST        ( ST
                               , runST
                               )
import Data.STRef              (STRef
                               , writeSTRef
                               , newSTRef
                               , readSTRef
                               )
import Prelude          hiding ((!!))

data Number
  = Int32 Int
  | HDouble Double

equalTypes :: Number -> Number -> Bool
equalTypes (Int32   _) (Int32   _) = True
equalTypes (HDouble _) (HDouble _) = True
equalTypes _           _           = False

instance Show Number where
  show (Int32   v) = show v
  show (HDouble v) = show v

class Show a => Convertable a where
  convert :: a -> Number 
  
instance Convertable Double where
  convert = HDouble

instance Convertable Int where
  convert = Int32


instance Eq Number where
  (Int32   v1) == (Int32   v2) = v1 == v2
  (HDouble v1) == (HDouble v2) = v1 == v2
  (Int32   v1) == (HDouble v2) = (fromIntegral v1) == v2
  (HDouble v1) == (Int32   v2) = v1 == (fromIntegral v2)

instance Ord Number where
  (Int32   v1) <= (Int32   v2) = v1 <= v2
  (HDouble v1) <= (HDouble v2) = v1 <= v2
  (Int32   v1) <= (HDouble v2) = (fromIntegral v1) <= v2
  (HDouble v1) <= (Int32   v2) = v1 <= (fromIntegral v2)

instance Num Number where
  (Int32   v1) + (Int32   v2) = Int32   $ v1 + v2
  (HDouble v1) + (HDouble v2) = HDouble $ v1 + v2
  (Int32   v1) + (HDouble v2) = HDouble $ (fromIntegral v1) + v2
  (HDouble v1) + (Int32   v2) = HDouble $ v1 + (fromIntegral v2)

  (Int32   v1) - (Int32   v2) = Int32   $ v1 - v2
  (HDouble v1) - (HDouble v2) = HDouble $ v1 - v2
  (Int32   v1) - (HDouble v2) = HDouble $ (fromIntegral v1) - v2
  (HDouble v1) - (Int32   v2) = HDouble $ v1 - (fromIntegral v2)

  (Int32   v1) * (Int32   v2) = Int32   $ v1 * v2
  (HDouble v1) * (HDouble v2) = HDouble $ v1 * v2
  (Int32   v1) * (HDouble v2) = HDouble $ (fromIntegral v1) * v2
  (HDouble v1) * (Int32   v2) = HDouble $ v1 * (fromIntegral v2)  

  fromInteger v = Int32 $ fromInteger v

  abs (Int32   v) = Int32   $ abs v
  abs (HDouble v) = HDouble $ abs v

  signum (Int32   v) = Int32   $ signum v
  signum (HDouble v) = HDouble $ signum v

class HScript code where

  (%+%) :: code Number -> code Number -> code Number
  (%-%) :: code Number -> code Number -> code Number
  (%*%) :: code Number -> code Number -> code Number

  (~=~)  :: code Number -> code Number -> code Bool
  (~/~)  :: code Number -> code Number -> code Bool
  (~<~)  :: code Number -> code Number -> code Bool
  (~>~)  :: code Number -> code Number -> code Bool
  (~<=~) :: code Number -> code Number -> code Bool
  (~>=~) :: code Number -> code Number -> code Bool
  -- | Strong equality check operator.
  -- First, it checks types of arguments and then values of arguments
  (~==~) :: code Number -> code Number -> code Bool
  (~/=~) :: code Number -> code Number -> code Bool

  (~&&~) :: code Bool -> code Bool -> code Bool
  (~||~) :: code Bool -> code Bool -> code Bool

  while :: code Bool -> code () -> code ()
  iff   :: code Bool -> code () -> code () -> code ()

  oneArgFunc :: (code Number -> code (Saver code Number) -> code ()) -> code Number -> code Number
  twoArgFunc :: (code Number -> code Number -> code (Saver code Number) -> code ()) -> code Number -> code Number -> code Number

  -- | Next-action operator. Something like ";" in JS
  (\\) :: code previous -> code next -> code next

  -- | Operator for creating new variable
  (##) :: Convertable t => t -> (code (Saver code Number) -> code ()) -> code ()

  -- | Operator for accessing variable value
  ($$) :: code (Saver code Number) -> code Number 

  -- | Operator for using pure types such as: `Int`, `String`, `Bool` in common expressions with variables
  (!!) :: Convertable t => t -> code Number

  (===) :: code (Saver code Number) -> code Number -> code ()
  (+=)  :: code (Saver code Number) -> code Number -> code ()
  (-=)  :: code (Saver code Number) -> code Number -> code ()
  (*=)  :: code (Saver code Number) -> code Number -> code ()

  -- | alias for `Boolean` True value
  -- true  :: code Bool
  -- -- | alias for `Boolean` False value
  -- false :: code Bool

  type Saver code t :: *

class Helpers code where
  applyBinaryFunc     :: (Number -> Number -> Number)  -> code Number -> code Number -> code Number
  applyCompareFunc    :: (Number -> Number -> Bool) -> code Number -> code Number -> code Bool
  applyChangeVarFunc  :: (code Number -> code Number -> code Number) -> code (Saver code Number) -> code Number -> code ()

stVar :: a -> ST code a
stVar v = return v

instance Helpers (ST code) where

  applyBinaryFunc f inputValue1 inputValue2 = do
    value1 <- inputValue1
    value2 <- inputValue2
    return $ value1 `f` value2

  applyCompareFunc f inputValue1 inputValue2 = do
    value1 <- inputValue1
    value2 <- inputValue2
    return $ value1 `f` value2

  applyChangeVarFunc f inputVar inputValue = do
    var <- inputVar
    res <- ($$) inputVar `f` inputValue
    writeSTRef var res

instance HScript (ST code) where

  (%+%) = applyBinaryFunc (+)
  (%-%) = applyBinaryFunc (-)
  (%*%) = applyBinaryFunc (*)

  (~=~)  = applyCompareFunc (==)
  (~/~)  = applyCompareFunc (/=)
  (~<~)  = applyCompareFunc (<)
  (~>~)  = applyCompareFunc (>)
  (~<=~) = applyCompareFunc (<=)
  (~>=~) = applyCompareFunc (>=)

  (~==~) inputValue1 inputValue2 = do
    value1 <- inputValue1
    value2 <- inputValue2
    return $ (equalTypes value1 value2) && (value1 == value2)
  (~/=~) inputValue1 inputValue2 = do
    value1 <- inputValue1
    value2 <- inputValue2
    return $ (not $ equalTypes value1 value2) || (value1 /= value2)

  (~&&~) inputValue1 inputValue2 = do
    value1 <- inputValue1
    value2 <- inputValue2
    return $ value1 && value2
  (~||~) inputValue1 inputValue2 = do
    value1 <- inputValue1
    value2 <- inputValue2
    return $ value1 || value2

  while inputCondition toEval = do
    condition <- inputCondition
    if condition
      then do
        toEval
        while inputCondition toEval
      else return ()

  iff inputCondition trueEval falseEval = do
    condition <- inputCondition
    if condition then trueEval else falseEval

  oneArgFunc toEval a = do
    returnValue <- newSTRef 0
    toEval a $ stVar returnValue
    readSTRef returnValue

  twoArgFunc toEval a1 a2 = do
    returnValue <- newSTRef 0
    toEval a1 a2 $ stVar returnValue
    readSTRef returnValue

  (\\) = (>>)

  (##) newVar next = do
    var <- newSTRef $ convert newVar
    next $ stVar var  

  ($$) inputVar = do
    var <- inputVar
    readSTRef var

  (!!) v = do
    return $ convert v

  (===) = applyChangeVarFunc (\_ v -> v)
  (+=)  = applyChangeVarFunc (%+%)
  (-=)  = applyChangeVarFunc (%-%)
  (*=)  = applyChangeVarFunc (%*%)

  -- true  = return $ Boolean True
  -- false = return $ Boolean False

  type Saver (ST code) t = STRef code t

infixl 1 \\
infix  3 ~&&~
infix  3 ~||~
infix  4 ===
infix  4 +=
infix  4 -=
infix  4 *=
infixr 9 $$
infixr 9 !!

-- | Some chaotic actions showing working with different data types in HalyavaScript
mainFunc :: HScript code => code Number -> code Number -> code Number
mainFunc = twoArgFunc $ \a1 a2 returnValue -> 
  (##) (22 :: Int) $ \funny ->
  (##) (33 :: Int) $ \scared ->
  funny *= (!!) (555 :: Int) \\
  scared += (!!) (42.2 :: Double) %+% ($$) scared \\
  funny -= (!!) (100 :: Int) \\
    iff (a1 ~=~ ($$) funny ~&&~ a2 ~=~ ($$) scared)
      (returnValue === (!!) (100 :: Int))
      (returnValue === (!!) (-100 :: Int))

-- | Recomended default arguments: (12110 :: Int, 108.2 :: Double)
runMainFunc :: (Convertable a, Convertable b) => a -> b -> Number
runMainFunc x1 x2 = runST $ mainFunc (stVar $ convert x1) $ stVar $ convert x2

-- | For a given @x@ calculates @ceiling (log2 (a))@
log2 :: HScript code => code Number -> code Number
log2 = oneArgFunc $ \a logCnt ->
  (##) (0 :: Int) $ \accum ->
  accum === (!!) (1 :: Int) \\
  logCnt === (!!) (0 :: Int) \\
  while (a ~>~ ($$) accum)
    ( accum *= (!!) (2 :: Int) \\
      logCnt += (!!) (1 :: Int)
    )

runLog2 :: Convertable a => a -> Number
runLog2 x = runST $ log2 $ stVar $ convert x