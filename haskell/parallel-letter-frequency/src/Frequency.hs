module Frequency
  ( frequency
  ) where

import           Data.Map  (Map)

import qualified Data.Char as C
import qualified Data.Map  as M

import           Data.Text (Text)
import qualified Data.Text as T
import Control.Parallel.Strategies

frequency :: Int -> [Text] -> Map Char Int
frequency nWorkers texts = M.unionsWith (+) mapsP
  where
    maps = map (charCount . T.toLower . letterFilter) texts
    letterFilter = T.filter C.isLetter
    chunkSize = length texts `div` nWorkers
    mapsP = maps `using` parListChunk chunkSize rdeepseq

-- Given a string, convert it to a map of character frequency counts
charCount :: Text -> M.Map Char Int
charCount = T.foldl addCharToMap M.empty

-- Build up a map of character frequency counts by adding one character seen.
addCharToMap :: M.Map Char Int -> Char -> M.Map Char Int
addCharToMap aMap x = M.insertWith (+) x 1 aMap
