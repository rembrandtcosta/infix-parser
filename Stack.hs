module Stack (Stack(..), empty, push, pop, top) where

    import Data.Maybe 

    data Stack a = Stack [a] deriving Show

    empty :: Stack a
    empty = Stack [] 

    push :: a -> Stack a -> Stack a
    push x (Stack xs) = Stack (x:xs)

    pop :: Stack a -> Maybe (Stack a)
    pop (Stack []) = Nothing
    pop (Stack (x:xs)) = Just $ Stack xs

    top :: Stack a -> Maybe a 
    top (Stack []) = Nothing
    top (Stack (x:xs)) = Just x