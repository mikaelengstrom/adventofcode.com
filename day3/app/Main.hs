module Main where

import Lib

filterValidTriangles :: [Triangle] -> [Triangle]
filterValidTriangles triangles =
  filter isValidTriangle triangles


main :: IO ()
main = do
  contents <- readFile "data/data"

  let trianglesStep1 = filterValidTriangles (parse contents)
  let trianglesStep2 = filterValidTriangles (columnParser contents)

  putStrLn "Valid triangle count (step 1): "
  putStrLn $ show $ length trianglesStep1

  putStrLn "Valid triangle count (step 2): "
  putStrLn $ show $ length trianglesStep2
