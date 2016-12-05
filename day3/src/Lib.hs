module Lib
    ( isValidTriangle
    , listToTriangle
    , Triangle
    , parse
    , columnParser
    ) where

import Data.List

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


columnParser x = makeTuples $ fmap snd $ order $ setColumns $ words x
  where setColumns ws = [(mod i 3, (read v :: Int)) | (i, v) <- zip [0..(length ws)] ws]
        order xs = sortBy (\a b -> if (fst a > fst b) then GT else LT) xs
        makeTuples [] = []
        makeTuples (x:y:z:xs) = (x, y, z):makeTuples xs
