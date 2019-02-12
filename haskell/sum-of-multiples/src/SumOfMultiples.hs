module SumOfMultiples
  ( sumOfMultiples
  ) where

import           Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ nub $ concatMap (multiples limit) factors

multiples :: Integer -> Integer -> [Integer]
multiples limit 0      = []
multiples limit factor = [i | i <- [1 .. (limit - 1)], i `mod` factor == 0]
