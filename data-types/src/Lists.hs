module Lists where

getLeftHalf :: [arr] -> [arr]
getLeftHalf arr = take (length arr `div` 2) arr

getRightHalf :: [arr] -> [arr]
getRightHalf arr = drop (length arr `div` 2) arr

getHalf :: [arr] -> ([arr], [arr])
getHalf arr = (getLeftHalf arr, getRightHalf arr)
