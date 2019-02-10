module DNA
  ( toRNA
  ) where

toRNA :: String -> Either Char String
toRNA xs = concat <$> traverse toRNA1 xs

toRNA1 :: Char -> Either Char String
toRNA1 x =
  case x of
    'C' -> Right "G"
    'G' -> Right "C"
    'T' -> Right "A"
    'A' -> Right "U"
    _   -> Left x
