module Lecture7 where

import Parsing -- see first version for implementation without Parsing module

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
