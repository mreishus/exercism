module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys 
  | valid = Just $ length $ filter (== False) $ zipWith (==) xs ys
  | otherwise = Nothing
  where
    valid = length xs == length ys
