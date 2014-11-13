module Lecture7 where

import Parsing -- see first version for implementation without Parsing module

p :: Parser (Char, Char)

p = item >>= \x ->
    item >>= \_ ->
    item >>= \y ->
    return (x,y) 
    
-- parse p "abcd"