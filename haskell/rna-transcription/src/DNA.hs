module DNA
  ( toRNA
  ) where

toRNA :: String -> Either Char String
toRNA = mapM toRNA1
  where
    toRNA1 x =
      case x of
        'C' -> Right 'G'
        'G' -> Right 'C'
        'T' -> Right 'A'
        'A' -> Right 'U'
        _   -> Left x
