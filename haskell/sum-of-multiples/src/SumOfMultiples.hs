module SumOfMultiples
  ( sumOfMultiples
  ) where

import           Data.List.Extra (nubOrd)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit =
  sum $ nubOrd $ concatMap (multiples limit) factors

multiples :: Integer -> Integer -> [Integer]
multiples lim factor
  | factor == 0 = []
  | otherwise = [factor,(factor * 2) .. (lim - 1)]
