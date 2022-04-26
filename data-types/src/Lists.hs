module Lists where

import Debug.Trace

lhalfl :: [a] -> [a]
lhalfl arr = take (length arr `div` 2) arr

lhalfr :: [a] -> [a]
lhalfr arr = drop (length arr `div` 2) arr

lhalves :: [a] -> ([a], [a])
lhalves arr = (lhalfl arr, lhalfr arr)

lor :: [Bool] -> Bool
lor arr = foldr (||) False arr

land :: [Bool] -> Bool
land arr = foldr (&&) True arr

lany :: (a -> Bool) -> [a] -> Bool
lany f arr = foldr (\arx res -> f arx || res) False arr

lcontains :: Eq a => a -> [a] -> Bool
lcontains x arr = lany (== x) arr
