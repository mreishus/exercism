module Anagram (anagramsFor) where

import qualified Data.Map.Strict as Map
import qualified Data.Char

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter (\x -> areAnagrams x xs) xss

-- Are two words Anagrams?
areAnagrams :: String -> String -> Bool
areAnagrams s1 s2 = charCountI s1 == charCountI s2 && lowerCase s2 /= lowerCase s1

-- Constant: An empty map
emptyMap :: Num a => Map.Map Char a
emptyMap = Map.fromList []

-- Given a string, convert it to a map of character frequency counts
charCount :: Num a => String -> Map.Map Char a
charCount xs = foldl addCharToMap emptyMap xs

-- Build up a map of character frequency counts by adding one character seen.
addCharToMap :: Num a => Map.Map Char a -> Char -> Map.Map Char a
addCharToMap aMap x = Map.insertWith (+) x 1 aMap

-- Case insensitive version. Given a string, convert it to a map of character frequency counts
charCountI :: Num a => String -> Map.Map Char a
charCountI xs = charCount $ lowerCase xs

-- Return a lowercase version of a String
lowerCase :: String -> String
lowerCase xs = map (\x -> Data.Char.toLower x) xs

