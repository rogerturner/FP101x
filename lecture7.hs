module Lecture7 where

type Parser a = String -> [(a, String)]

item :: Parser Char
item  = \inp -> case inp of
                  []     -> []
                  (x:xs) -> [(x,xs)]
                  
failure :: Parser a
failure  = \inp -> []

return'  :: a -> Parser a
return' v = \inp -> [(v,inp)]

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp

(+++)  :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case parse p inp of
                    []        -> parse q inp
                    [(v,out)] -> [(v,out)]

moveOnSavingAs      :: Parser a -> (a -> Parser b) -> Parser b  -- " >>= "
p `moveOnSavingAs` f = \inp -> case parse p inp of  -- apply p
                []        -> []                     -- p failed: fail
                [(v,out)] -> parse (f v) out
                -- p succeeded with result v (and remaining input):
                -- apply a wrapper which saves v and then applies next parser

p :: Parser (Char, Char)

p = item `moveOnSavingAs` \x ->
    item `moveOnSavingAs` \_ ->
    item `moveOnSavingAs` \y ->
    return' (x,y)