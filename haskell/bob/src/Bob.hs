{-# LANGUAGE OverloadedStrings #-}

module Bob
  ( responseFor
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

data Statement
  = Silent
  | YellQuestion
  | Yell
  | Question
  | Normal
  deriving (Show, Eq)

responseFor :: Text -> Text
responseFor xs
  | s == Silent = "Fine. Be that way!"
  | s == YellQuestion = "Calm down, I know what I'm doing!"
  | s == Yell = "Whoa, chill out!"
  | s == Question = "Sure."
  | otherwise = "Whatever."
  where
    s = parseStatement xs

parseStatement :: Text -> Statement
parseStatement xs'
  | isSilent xs = Silent
  | isYell xs && isQuestion xs = YellQuestion
  | isYell xs = Yell
  | isQuestion xs = Question
  | otherwise = Normal
  where xs = T.strip xs'

isSilent :: Text -> Bool
isSilent xs = T.length xs == 0

-- Does input contain at least one uppercase letter and no lowercase letters?
isYell :: Text -> Bool
isYell xs = xsUpper == xs && xsUpper /= xsLower
  where
    xsUpper = T.toUpper xs
    xsLower = T.toLower xs

isQuestion :: Text -> Bool
isQuestion xs = T.last xs == '?'
