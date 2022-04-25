module Strings where

import Data.Char

toInt :: String -> Int
toInt str
    | null str              = 0
    | not $ all isDigit str = 0
    | otherwise             = read str :: Int
