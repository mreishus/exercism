module CollatzConjecture
  ( collatz
  ) where

import Data.List (unfoldr, genericLength)

-- dupe: Turns a single value into a tuple of that value repeated.
dupe :: a -> (a,a)
dupe x = (x,x)

-- nextCollatz:  Finds the next number in the collatz sequence, or Nothing if done.
-- Number is returned as a duplicate tuple so this can be used with unfoldr.
nextCollatz :: Integer -> Maybe (Integer, Integer)
nextCollatz n
  | n <= 1 = Nothing
  | n `mod` 2 == 0 = Just $ dupe $ n `div` 2
  | otherwise = Just $ dupe $ n * 3 + 1

-- collatz: Find the length of the collatz sequence beginning with n.
collatz :: Integer -> Maybe Integer
collatz n
  | n < 1 = Nothing
  | otherwise = Just $ genericLength $ unfoldr nextCollatz n

{-|

Recursive version w/ "go" function

collatz :: Integer -> Maybe Integer
collatz x = go x 0
  where
    go n steps
      | n < 1 = Nothing
      | n == 1 = Just steps
      | n `mod` 2 == 0 = go (n `div` 2) (steps + 1)
      | otherwise = go (n * 3 + 1) (steps + 1)
-}
