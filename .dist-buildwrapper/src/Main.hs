module Main where
import Control.Monad

infixr 5 +++

--Monad
newtype Parser a = Parser (String -> [(a , String)])

instance Monad Parser where
    return a = Parser (\inp -> [(a , inp)])
    p >>= f  = Parser (\inp -> concat [parse (f v) inp' | (v,inp') <- parse p inp])


instance MonadPlus Parser where
    mzero     = Parser (\inp -> [])
    mplus p q = Parser (\inp -> parse p inp ++ parse q inp)

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
        else mzero
--combinators
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser (\inp -> case parse (mplus p q) inp of
                    [] -> []
                    (x:_) -> [x])

char :: Char -> Parser Char
char c = sat (c == )
                    
string :: String -> Parser String
string "" = return ""
string (x:xs) = do
        _ <- char x
        _ <- string xs
        return (x:xs)

many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do
      a <- p
      ass <- many p
      return (a : ass)
      
main::IO()
main = undefined