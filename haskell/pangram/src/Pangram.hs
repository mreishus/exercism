{-# LANGUAGE OverloadedStrings #-}

module Pangram (isPangram) where

import qualified Data.Map.Strict as M
import qualified Data.Char as C

import           Data.Text (Text)
import qualified Data.Text as T

isPangram :: Text -> Bool
isPangram text = (M.size $ charCount filteredText ) == 26
    where
        filteredText = T.map C.toLower $ T.filter C.isLetter $ text

-- Constant: An empty map
emptyMap :: Num a => M.Map Char a
emptyMap = M.fromList []

-- Given a string, convert it to a map of character frequency counts
charCount :: Num a => Text -> M.Map Char a
charCount xs = T.foldl addCharToMap emptyMap xs

-- Build up a map of character frequency counts by adding one character seen.
addCharToMap :: Num a => M.Map Char a -> Char -> M.Map Char a
addCharToMap aMap x = M.insertWith (+) x 1 aMap
