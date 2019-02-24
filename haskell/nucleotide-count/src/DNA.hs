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
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = nucCount <$> parse xs

parse :: String -> Either String [Nucleotide]
parse = mapM (\x -> readEither [x])

nucCount :: [Nucleotide] -> Map Nucleotide Int
nucCount = foldl addNucToMap noNucs

noNucs :: Map Nucleotide Int
noNucs = M.fromList $ zip [(minBound :: Nucleotide) ..] [0,0 ..]

-- Build up a map of nuc frequency counts by adding one nuc seen.
addNucToMap :: Map Nucleotide Int -> Nucleotide -> Map Nucleotide Int
addNucToMap aMap x = M.insertWith (+) x 1 aMap
