module Triangle
  ( TriangleType(..)
  , triangleType
  ) where

import           Data.List (nub, sort)

data TriangleType
  = Equilateral
  | Isosceles
  | Scalene
  | Illegal
  deriving (Eq, Show)

triangleType :: (Ord a, Eq a, Num a) => a -> a -> a -> TriangleType
triangleType a b c
  | not valid = Illegal
  | uniqs == 1 = Equilateral
  | uniqs == 2 = Isosceles
  | uniqs == 3 = Scalene
  where
    uniqs = length $ nub [a, b, c]
    sorted = sort [a, b, c]
    valid = sorted !! 2 < (sorted !! 1 + sorted !! 0)
