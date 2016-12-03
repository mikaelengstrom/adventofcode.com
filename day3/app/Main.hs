module Main where

import Lib

filterValidTriangles :: [Triangle] -> [Triangle]
filterValidTriangles triangles =
  filter isValidTriangle triangles


main :: IO ()
main = do
  contents <- readFile "data/data"

  let triangles = filterValidTriangles (parse contents)

  putStrLn $ show triangles

  putStrLn "Valid triangle count: "
  putStrLn $ show $ length triangles
