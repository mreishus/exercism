module Bob (responseFor) where

import qualified Data.Char as DC

data Statement = Silent | YellQuestion | Yell | Question | Normal 
     deriving (Show, Eq)

responseFor :: String -> String
responseFor xs
    | s == Silent = "Fine. Be that way!"
    | s == YellQuestion = "Calm down, I know what I'm doing!"
    | s == Yell = "Whoa, chill out!"
    | s == Question = "Sure."
    | s == Normal = "Whatever."
    where s = parseStatement $ trim xs

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile DC.isSpace

parseStatement :: String -> Statement
parseStatement xs
    | isSilent xs = Silent
    | isYell xs && isQuestion xs = YellQuestion
    | isYell xs = Yell
    | isQuestion xs = Question
    | otherwise = Normal

isSilent :: String -> Bool
isSilent xs = length xs == 0

-- Does input contain at least one uppercase letter and no lowercase letters?
isYell :: String -> Bool
isYell xs = xsUpper == xs && xsUpper /= xsLower
    where
        xsUpper = map DC.toUpper xs
        xsLower = map DC.toLower xs

isQuestion :: String -> Bool
isQuestion xs = last xs == '?'
