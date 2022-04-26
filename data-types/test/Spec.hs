import Data.Char
import Lists
import Strings
import Test.QuickCheck

prop_toInt :: String -> Bool
prop_toInt str = if str == "" || (not $ all isDigit str)
    then True
    else toInt str == (read str :: Int)

prop_lhalves :: [String] -> Bool
prop_lhalves arr
    | null arr             = True
    | null $ arr!!0        = True
    | otherwise            = (length $ fst $ lhalves arr) == (length arr) `div` 2

prop_combineHalves :: [String] -> Bool
prop_combineHalves arr
    | null arr             = True
    | null $ arr!!0        = True
    | otherwise            = (fst $ halfp) ++ (snd $ halfp) == arr
    where halfp = lhalves arr

prop_lor :: [Bool] -> Bool
prop_lor arr
    | null arr             = True
    | otherwise            = lor arr == any (== True) arr
    
prop_land :: [Bool] -> Bool
prop_land arr
    | null arr             = True
    | otherwise            = land arr == all (== True) arr

prop_lany :: [Int] -> Bool
prop_lany arr
    | null arr             = True
    | otherwise            = lany even arr == any even arr && lany odd arr == any odd arr

prop_lcontains :: Int ->[Int] -> Bool
prop_lcontains elem arr
    | null arr             = True
    | otherwise            = lcontains elem arr == any (== elem) arr

main :: IO ()
main = do
    quickCheck prop_toInt
    quickCheck prop_lhalves
    quickCheck prop_combineHalves
    quickCheck prop_lor
    quickCheck prop_land
    quickCheck prop_lany
    quickCheck prop_lcontains
    putStrLn "OK"
