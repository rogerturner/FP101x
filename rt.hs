module RT where

(|>) :: a -> (a -> b) -> b ; infixl 0 |>
a |> f = f a  -- pipeline operator
