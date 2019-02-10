module DNA
  ( toRNA
  ) where

import           Data.Either (partitionEithers)

toRNA :: String -> Either Char String
toRNA xs
  | lefts == "" = Right $ concat rights
  | otherwise = Left $ head lefts
  where
    (lefts, rights) = partitionEithers $ map toRNA1 xs

toRNA1 :: Char -> Either Char String
toRNA1 x =
  case x of
    'C' -> Right "G"
    'G' -> Right "C"
    'T' -> Right "A"
    'A' -> Right "U"
    _   -> Left x
