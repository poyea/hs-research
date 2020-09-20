module Main where

import Lib ( checkNumber )
import System.Exit ( exitSuccess )

main = askForNumber

askInvalid = putStrLn "Invalid integer"
askGuess = putStrLn "Input a target: "

askForNumber :: IO ()
askForNumber = do
  askGuess
  target <- getLine
  if null target
    then do
      askInvalid
      askForNumber
    else do
      let targetNumber = (read target :: Int)
      putStrLn "Here we go..."
      goGuess targetNumber True

goGuess :: Int -> Bool -> IO ()
goGuess target signal = do
  if not signal
    then exitSuccess
    else do
      askGuess
      number <- getLine
      if null number
        then do
          askInvalid
          goGuess target True
        else do
          let theNumber = (read number :: Int)
          putStrLn $ checkNumber theNumber target
          goGuess target (theNumber /= target)