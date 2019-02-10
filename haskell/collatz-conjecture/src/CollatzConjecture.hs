module CollatzConjecture
  ( collatz
  ) where

import           Data.List (genericLength)

-- nextCollatz:  Finds the next number in the collatz sequence
nextCollatz :: Integer -> Integer
nextCollatz n
  | n <= 1 = n
  | n `mod` 2 == 0 = n `quot` 2
  | otherwise = n * 3 + 1

-- collatz: Find the length of the collatz sequence beginning with n.
collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | otherwise = Just $ genericLength $ takeWhile (/= 1) $ iterate nextCollatz n
