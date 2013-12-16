module Main where

infixr 5 +++

--Monad
newtype Parser a = Parser (String -> [(a , String)])

instance Monad Parser where
    return a = Parser (\inp -> [(a , inp)])
    p >>= f  = Parser (\inp -> concat [parse (f v) inp' | (v,inp') <- parse p inp])
instance MonadZero Parser where
    zero = Parser (\inp -> [])

instance MonadPlus Parser where
    p ++ q = Parser (\inp -> parse p inp ++ parse q inp)

--parsing primitives
parse :: Parser a -> (String -> [(a , String)])
parse (Parser p) = p

item :: Parser Char
item = Parser (\inp -> case inp of
                       "" -> []
                       (x:xs) -> [(x,xs)])
sat :: (Char -> Bool) -> Parser Char
sat p = do 
    c <- item
    if p c then
        return c
        else zero
--combinators


main::IO()
main = undefined