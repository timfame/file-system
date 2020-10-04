{-# LANGUAGE InstanceSigs #-}

module Block1.Task1 where

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving Show

instance Eq Day where
	(==) :: Day -> Day -> Bool
	(==) Monday Monday       = True
	(==) Tuesday Tuesday     = True
	(==) Wednesday Wednesday = True
	(==) Thursday Thursday   = True
	(==) Friday Friday       = True
	(==) Saturday Saturday   = True
	(==) Sunday Sunday       = True
	(==) _ _                 = False

nextDay :: Day -> Day
nextDay Monday = Tuesday
nextDay Tuesday = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday = Friday
nextDay Friday = Saturday
nextDay Saturday = Sunday
nextDay Sunday = Monday

isWeekend :: Day -> Bool
isWeekend Saturday = True
isWeekend Sunday = True
isWeekend _ = False

afterDays :: Day -> Int -> Day
afterDays currentDay 0 = currentDay
afterDays currentDay afterCount = nextDay (afterDays currentDay (afterCount - 1))

daysToParty :: Day -> Int
daysToParty Friday = 0
daysToParty currentDay = (daysToParty (nextDay currentDay)) + 1