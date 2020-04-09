module Acronym (abbreviate) where
import Data.Char

split :: String -> String -> [String] -> [String]
split (h:t) a r 
        | elem h [' ', '-'] = split t [] (r ++ [a])
        | otherwise = split t (a ++ [h]) r
split [] a r = r ++ [a]

letters :: String -> String
letters a@(h:t)
        | all isUpper a = [toUpper h]
        | otherwise = [toUpper h] ++ [x | x <- t, elem x ['A'..'Z']]
letters [] = ""

abbreviate :: String -> String
abbreviate xs =
    concat [letters x | x <- split xs [] []]