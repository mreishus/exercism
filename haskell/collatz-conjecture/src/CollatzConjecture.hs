module CollatzConjecture
  ( collatz
  ) where

collatz :: Integer -> Maybe Integer
collatz x = go x 0
  where
    go n steps
      | n < 1 = Nothing
      | n == 1 = Just steps
      | n `mod` 2 == 0 = go (n `div` 2) (steps + 1)
      | otherwise = go (n * 3 + 1) (steps + 1)
