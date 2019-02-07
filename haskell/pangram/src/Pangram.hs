{-# LANGUAGE OverloadedStrings #-}

module Pangram
  ( isPangram
  ) where

import qualified Data.Char as C
import qualified Data.Set  as S

import           Data.Text (Text)
import qualified Data.Text as T

isPangram :: Text -> Bool
isPangram text = (S.size $ charSet filteredText) == 26
  where
    filteredText = T.map C.toLower $ T.filter C.isLetter $ text
    charSet xs = T.foldl (flip S.insert) S.empty xs
