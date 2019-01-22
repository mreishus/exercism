module Acronym (abbreviate) where

import qualified Data.Char (toUpper, isUpper, isLower)

abbreviate :: String -> String
abbreviate xs = concat $ map abbrev_word $ Prelude.words $ dash_to_space xs

-- Converts dashes to spaces.
dash_to_space :: String -> String
dash_to_space xs = map (\c -> if c=='-' then ' '; else c) xs

-- Given a word, abbreviates it.
-- Usually picks the first letter and capitalizes it: "Cat" -> "C"
-- But we look for camelcase and return all uppercase if so. "HyperText" -> "HT"
abbrev_word :: String -> String
abbrev_word w = go w (num_upper w) (num_lower w)
  where
    go _ u l | u > 1 && l > 1 = filter Data.Char.isUpper w
             | otherwise      = [Data.Char.toUpper(head w)]

-- Number of uppercase chars in a string.
num_upper :: String -> Int
num_upper w = length $ filter Data.Char.isUpper w

-- Number of lowercase chars in a string.
num_lower :: String -> Int
num_lower w = length $ filter Data.Char.isLower w


-- Bonus:
-- Naive Algorithm that fails on "HyperText" -> "HT"
--
-- abbreviate :: String -> String
-- abbreviate xs = map (\x -> Data.Char.toUpper(head x)) $ Prelude.words xs
