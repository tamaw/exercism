module CryptoSquare (encode) where

import Data.Char
import Data.List

encode :: String -> String
encode xs = unwords . transpose . take r $ split c ("", s ++ [' ',' '..])
    where s = normalise xs
          c = chunks s
          l = length s
          r = rows l c

normalise :: String -> String
normalise s = filter isAlphaNum $ map toLower s

chunks :: String -> Int
chunks s = ceiling . sqrt . fromIntegral $ length s

rows :: Int -> Int -> Int
rows length chunk = ceiling $ l / c
    where l = fromIntegral length
          c = fromIntegral chunk

split :: Int -> (String, String) -> [String]
split _ ([],[]) = []
split l ([],xs) = split l (splitAt l xs) 
split l (x,xs) = x:split l (splitAt l xs) 
