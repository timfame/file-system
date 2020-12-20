module Block2.Task1
    ( Expr(..),
      ArithmeticError(..),
      eval
    ) where

import Control.Monad (liftM2)

data Expr 
    = Const Int
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Pow Expr Expr
    deriving (Show)

data ArithmeticError
    = DivisionByZero
    | NegatePower
    deriving (Show, Eq)

checkMaybeOnNegate :: Either ArithmeticError Int -> Bool
checkMaybeOnNegate (Right x) = x < 0
checkMaybeOnNegate _         = False

eval :: Expr -> Either ArithmeticError Int
eval (Const v) = Right v
eval (Add l r) = liftM2 (+) (eval l) $ eval r
eval (Sub l r) = liftM2 (-) (eval l) $ eval r
eval (Mul l r) = liftM2 (*) (eval l) $ eval r
eval (Div l r) = case (eval r) of
    (Right 0) -> Left DivisionByZero
    rRes      -> liftM2 div (eval l) rRes
eval (Pow l r) = case (checkMaybeOnNegate $ eval r) of
    True  -> Left NegatePower
    False -> liftM2 (^) (eval l) $ eval r

