module CryptoSquare (encode) where

import qualified Data.Char as DC
import qualified Data.List.Split as DLS
import qualified Data.List as DL
import qualified Data.Strings as DS

encode :: String -> String
encode xs = printSquare $ DL.transpose $ toSquare $ normalize xs

toSquare :: String -> [String]
toSquare xs = pad c $ DLS.chunksOf c xs
    where (c, _r) = squareSize $ length xs

pad :: Int -> [String] -> [String]
pad n xs = map (\x -> DS.strPadRight ' ' n x) xs

printSquare :: [String] -> String
printSquare xs = DL.intercalate " " xs

-- Normalize: Only keep letters and numbers, downcase
normalize :: String -> String
normalize xs = map DC.toLower $ filter (\x -> DC.isNumber x || DC.isLetter x) xs

squareSize :: Int -> (Int, Int)
squareSize n = squareSize' n 1 1

squareSize' :: Int -> Int -> Int -> (Int, Int)
squareSize' n c r
    | n <= r * c = (c, r)
    | r == c = squareSize' n (c+1) r
    | otherwise = squareSize' n c (r+1)
