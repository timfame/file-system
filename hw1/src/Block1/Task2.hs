{-# LANGUAGE InstanceSigs #-}

module Block1.Task2 where

data Nat = Z | S Nat deriving Show

instance Eq Nat where
    (==) :: Nat -> Nat -> Bool
    (==) Z Z = True
    (==) (S first) (S second) = first == second
    (==) _ _ = False

instance Ord Nat where
	(<=) :: Nat -> Nat -> Bool
	(<=) Z Z = True
	(<=) (S _) Z = False
	(<=) Z (S _) = True
	(<=) (S first) (S second) = first <= second

instance Num Nat where
	(+) :: Nat -> Nat -> Nat
	(+) Z value = value
	(+) value Z = value
	(+) first (S second) = S ((+) first second)

	(*) :: Nat -> Nat -> Nat
	(*) Z _ = Z
	(*) _ Z = Z
	(*) first (S second) = (first * second) + first

	(-) :: Nat -> Nat -> Nat
	(-) value Z = value
	(-) (S first) (S second) = first - second
	(-) Z _ = error "Result is not natural"

	fromInteger :: Integer -> Nat
	fromInteger 0 = Z
	fromInteger value = S (fromInteger (value - 1))

	abs :: Nat -> Nat
	abs value = value

	signum :: Nat -> Nat
	signum _ = S Z

natToInteger :: Nat -> Integer
natToInteger Z = 0
natToInteger (S value) = 1 + natToInteger value

divNat :: Nat -> Nat -> Nat
divNat first second | second == Z = error ""
				  | second > first = Z
				  | otherwise = S (divNat (first - second) second)

modNat :: Nat -> Nat -> Nat
modNat _ Z = error "Divison by zero"
modNat first second = first - (divNat first second) * second