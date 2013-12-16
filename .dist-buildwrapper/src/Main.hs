module Main where


type Parser a = String -> [(a , String)]

--data Parser a = Parser (String -> [(a , String)])

result :: a -> Parser a
result v = \inp -> [(v , inp)]

zero :: Parser a
zero = \inp -> []

item :: Parser Char
item = \inp  case inp of
        [] -> []
        (x : xs) -> [(x , xs)]
        
        
seq :: Parser a -> Parser b -> Parser (a , b)
a `seq` b = \inp -> [((v1,v2) , inp'') | (v1 , inp') <- a inp , (v2,inp'') <- b inp']

bind :: Parser a -> (a -> Parser b) -> Parser b
p `bind` f = \inp -> concat [f v inp' | (v ,inp') <- p inp]

plus :: Parser a -> Parser a -> Parser a
p `plus` q = \inp -> (p inp ++ q inp)

sat :: (Char -> Bool) -> Parser Char
sat p = item `bind` \x ->
        if p x then result x else zero

char :: Char -> Parser Char
char x = sat (\y -> x == y)

main::IO()
main = undefined