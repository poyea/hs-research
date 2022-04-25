import Data.Char
import Lists
import Strings
import Test.QuickCheck

prop_toInt :: String -> Bool
prop_toInt str = if str == "" || (not $ all isDigit str)
    then True
    else toInt str == (read str :: Int)

prop_getHalf :: [String] -> Bool
prop_getHalf arr
    | null arr             = True
    | null $ arr!!0        = True
    | otherwise            = (length $ fst $ getHalf arr) == (length arr) `div` 2

prop_combineHalves :: [String] -> Bool
prop_combineHalves arr
    | null arr             = True
    | null $ arr!!0        = True
    | otherwise            = (fst $ halves) ++ (snd $ halves) == arr
    where halves = getHalf arr
    
main :: IO ()
main = do
    quickCheck prop_toInt
    quickCheck prop_getHalf
    quickCheck prop_combineHalves
    putStrLn "OK"
