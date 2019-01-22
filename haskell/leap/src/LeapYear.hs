module LeapYear
  ( isLeapYear
  ) where

isDivisibleBy :: Integer -> Integer -> Bool
isDivisibleBy year divisor = year `rem` divisor == 0

isLeapYear :: Integer -> Bool
isLeapYear year =
  let is_4 = isDivisibleBy year 4
      is_100 = isDivisibleBy year 100
      is_400 = isDivisibleBy year 400
   in is_4 && (not is_100 || is_400)
