module LeapYear
  ( isLeapYear
  ) where

isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy year divisor = year `rem` divisor == 0

isLeapYear :: Integer -> Bool
isLeapYear year
    | year `isDivisibleBy` 400 = True
    | year `isDivisibleBy` 100 = False
    | year `isDivisibleBy` 4 = True
    | otherwise = False
