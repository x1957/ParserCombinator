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
      
sepBy :: Parser a -> Parser b -> Parser [a]
p `sepBy` sep = (p `sepBy1` sep) +++ return []

sepBy1 :: Parser a -> Parser b -> Parser [a]
p `sepBy1` sep = do
  a <- p
  ass <- many (do {_<-sep ; p})
  return (a : ass)

chainl  :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a)  -> Parser a
p `chainl1` op = do {a <- p ; rest a}
                 where
                    rest a = (do f <- op
                                 b <- p
                                 rest (f a b)) +++ return a

--lexical combinators
space :: Parser String
space = many (sat isSpace)

token :: Parser a -> Parser a
token p = do {a <- p ; space ; return a}

symb :: String -> Parser String
symb cs = token (string cs)

num :: Parser Char
num = sat isNum

digit :: Parser Int
digit = do{space; n <- many1 num ; return $ read n}

apply :: Parser a -> String -> [(a , String)]
apply p = parse (do {space ; p})


--util
isSpace :: Char -> Bool
isSpace a = ' ' == a

isNum :: Char -> Bool
isNum a = a >= '0' && a <= '9'

--expr
expr :: Parser Int
expr = term `chainl1` addop

term = factor `chainl1` mulop
factor = digit +++ do {space;symb "(" ;space; n <- expr ;space; symb ")" ;space; return n}


addop :: Parser (Int -> Int -> Int)
addop = do {space;symb "+"; space; return (+)} +++ do {space;symb "-";space ; return (-)}

mulop :: Parser (Int -> Int -> Int)
mulop = do {space;symb "*" ; space ; return (*)} +++ do {space;symb "/" ; space ; return (div)}


main :: IO ()
main = undefined

