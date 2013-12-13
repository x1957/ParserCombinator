module Main where


type Paser a = String -> [(a , String)]

result :: a -> Parser a
result v = \inp -> [(v , inp)]

zero :: Parser a
zero = \inp -> []

item :: Parser Char
item = \inp  case inp of
        [] -> []
        (x : xs) -> [(x , xs)]
        
        
seq :: Parser a -> Parser b -> Parser (a , b)

main::IO()
main = undefined