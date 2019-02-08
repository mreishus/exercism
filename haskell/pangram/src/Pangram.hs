{-# LANGUAGE OverloadedStrings #-}

module Pangram
  ( isPangram
  ) where

import qualified Data.Char as C
import qualified Data.Set  as S

isPangram :: String -> Bool
isPangram text = elem 26 $ map S.size charSet
  where
    filteredText = map C.toLower $ filter C.isLetter $ text
    charSet = scanl (flip S.insert) S.empty filteredText
