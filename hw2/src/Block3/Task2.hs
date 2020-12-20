module Block3.Task2
    ( ok, 
      eof, 
      satisfy, 
      element, 
      stream
    ) where

import Block3.Task1 (Parser(..))

ok :: Parser s ()
ok = Parser $ \i -> Just ((), i)

eof :: Parser s ()
eof = Parser $ \i -> case i of
    [] -> Just((), i)
    _  -> Nothing
 
satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \i -> case i of
    []     -> Nothing
    (x:xs) -> if p x then Just (x, xs) else Nothing

element :: Eq e => e -> Parser e e
element o = satisfy (== o)

stream :: Eq e => [e] -> Parser e [e]
stream x = traverse element x

