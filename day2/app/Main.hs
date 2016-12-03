module Main where

import Lib

main :: IO ()
main = do
  contents <- readFile "data/task1.data"
  putStrLn $ solve contents
  return ()
