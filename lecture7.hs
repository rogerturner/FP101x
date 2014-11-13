module Lecture7 where

import Parsing -- see first version for implementation without Parsing module

p :: Parser (Char, Char)

p = do x <-item 
       item 
       y <- item 
       return (x,y) 
    
-- parse p "abcd"