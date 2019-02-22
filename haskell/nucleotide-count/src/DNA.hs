module DNA
  ( nucleotideCounts
  , Nucleotide(..)
  ) where

import           Data.Map  (Map)
import qualified Data.Map  as M
import           Text.Read (readEither)

data Nucleotide
  = A
  | C
  | G
  | T
  deriving (Eq, Ord, Show, Read)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = parse xs >>= nucCount

parse :: String -> Either String [Nucleotide]
parse = mapM (\x -> readEither [x])

nucCount :: [Nucleotide] -> Either String (Map Nucleotide Int)
nucCount nucs = Right $ foldl addNucToMap noNucs nucs

noNucs :: Map Nucleotide Int
noNucs = M.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

-- Build up a map of nuc frequency counts by adding one nuc seen.
addNucToMap :: Map Nucleotide Int -> Nucleotide -> Map Nucleotide Int
addNucToMap aMap x = M.insertWith (+) x 1 aMap
