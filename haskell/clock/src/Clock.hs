module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int deriving (Show, Eq)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock ((hour + roll_hours) `mod` 24) (min `mod` 60)
    where roll_hours = min `div` 60

toString :: Clock -> String
toString clock = showClock h ++ ":" ++ showClock m
    where (Clock h m) = clock

showClock :: Int -> String
showClock x
    | x < 10 = "0" ++ show x
    | otherwise = show x

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour1 min1 clock = 
    Clock hour3 min3
    where (Clock hour2 min2) = clock
          min3 = (min2 + min1) `mod` 60
          roll_hours = (min2 + min1) `div` 60
          hour3 = (hour2 + hour1 + roll_hours) `mod` 24
