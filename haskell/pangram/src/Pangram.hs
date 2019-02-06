module Pangram (isPangram) where

import qualified Data.Map.Strict as Map
import qualified Data.Char

isPangram :: String -> Bool
isPangram text = (Map.size $ charCount filteredText ) >= 26
    where
        filteredText = lowerCase $ filterLetter text

-- Constant: An empty map
emptyMap :: Num a => Map.Map Char a
emptyMap = Map.fromList []

-- Given a string, convert it to a map of character frequency counts
charCount :: Num a => String -> Map.Map Char a
charCount xs = foldl addCharToMap emptyMap xs

-- Build up a map of character frequency counts by adding one character seen.
addCharToMap :: Num a => Map.Map Char a -> Char -> Map.Map Char a
addCharToMap aMap x = Map.insertWith (+) x 1 aMap


-- Return a lowercase version of a String
lowerCase :: String -> String
lowerCase xs = map Data.Char.toLower xs

filterLetter :: String -> String
filterLetter xs = filter Data.Char.isLetter xs

