module Lecture7 where

import Parsing -- see first version for implementation without Parsing module

-- newtype Parser a = P (String -> [(a,String)])

p :: Parser (Char, Char)
p = do x <-item           -- parse p "abcd"
       item 
       y <- item 
       return (x,y) 
    
expr :: Parser Int
expr = do n <- natural    -- parse expr " 20 - 1-10"
          ns <- many
            (do symbol "-"
                natural)
          return (foldl (-) n ns)
          
lrexpr :: Parser Int    -- left recursive: non-terminating
lrexpr = ( do 
             n <- lrexpr
             symbol "-"
             m <- nat
             return (n - m))
         +++ ( do
                 n <- nat
                 return n )
                 
-- recexpr ::= recexpr - nat | nat
-- rewrite as:  recexpr     ::= nat subtrahends
--              subtrahends ::= - nat subtrahends | empty

recexpr :: Parser Int     -- recursive & left associative
recexpr = do
            n <- natural
            f <- subtrahends
            return (f n)
            
subtrahends :: Parser (Int -> Int)
subtrahends = ( do
                  symbol "-"
                  n <- natural
                  f <- subtrahends
                  return ( f . (\x -> x - n) )
              ) +++ ( do return id )
              
                