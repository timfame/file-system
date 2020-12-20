module Block1.Task1 
    ( stringSum
	) where

import Text.Read (readMaybe)

stringSum :: String -> Maybe Int
stringSum str = sum <$> traverse readMaybe (words str)