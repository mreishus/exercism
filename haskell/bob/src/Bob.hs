{-# LANGUAGE OverloadedStrings #-}

module Bob
  ( responseFor
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

responseFor :: Text -> Text
responseFor xs'
  | isSilent = "Fine. Be that way!"
  | isYell && isQuestion = "Calm down, I know what I'm doing!"
  | isYell = "Whoa, chill out!"
  | isQuestion = "Sure."
  | otherwise = "Whatever."
  where
    xs = T.strip xs'
    isSilent = T.null xs
    isQuestion = T.last xs == '?'
    isYell = T.toUpper xs == xs && T.toUpper xs /= T.toLower xs
