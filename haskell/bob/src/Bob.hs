{-# LANGUAGE OverloadedStrings #-}
module Bob
  ( responseFor
  ) where

import qualified Data.Text as T
import           Data.Text (Text)

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
    s = parseStatement $ T.strip xs

parseStatement :: Text -> Statement
parseStatement xs
  | isSilent xs = Silent
  | isYell xs && isQuestion xs = YellQuestion
  | isYell xs = Yell
  | isQuestion xs = Question
  | otherwise = Normal

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
