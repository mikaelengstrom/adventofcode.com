module Lib
    ( isValidTriangle
    , listToTriangle
    , Triangle
    , parse
    ) where

-- Geometry stuff

type Triangle = (Int, Int, Int)

isValidTriangle :: Triangle -> Bool
isValidTriangle (x, y, z) = totalSum - x > x && totalSum - y > y && totalSum - z > z
  where totalSum = x + y + z

listToTriangle :: [String] -> Triangle
listToTriangle (x:y:z:_) = (strToInt x, strToInt y, strToInt z)
  where strToInt str = read str :: Int


-- Parser

parse x = fmap parseLine $ lines x
  where parseLine = fmap listToTriangle words

