module Block3.Task1
    ( Parser(..)
    ) where

import Control.Applicative (Alternative, empty, (<|>))

data Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

instance Functor (Parser s) where
    fmap f p = Parser $ \i -> do
        (res, left) <- runParser p i
        return (f res, left)

instance Applicative (Parser s) where
    pure a    = Parser $ \i -> Just (a, i)
    pl <*> pr = do
        resl <- pl
        resr <- pr
        return (resl resr)


instance Monad (Parser s) where
    return res       = Parser $ \i -> return (res, i)
    (Parser p) >>= k = Parser $ \i -> do
        (firstRes, left) <- p i
        let (Parser newParser) = k firstRes
        (newRes, newLeft) <- newParser left
        return (newRes, newLeft)


instance Alternative (Parser s) where
    empty     = Parser $ \_ -> Nothing
    (Parser pl) <|> (Parser pr) = Parser $ \i -> (pl i <|> pr i) 