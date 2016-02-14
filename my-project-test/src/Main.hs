module Main where

import System.IO (readFile)
import Data.Time (getCurrentTime)

main :: IO ()
main = do
  putStrLn (greet "Oscar")
  putStrLn (greet "World")
greet name = "Hello " ++ name ++ "!"

printTime = do
  time <- getCurrentTime
  putStrLn (show time)

printConfig = do
  contents <- readFile "stack.yaml"
  putStrLn contents

printNumbers = do
  putStrLn (show (3 + 4))
