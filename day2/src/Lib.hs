module Lib
( tokenize
, Token(Move, Next)
, Pos
, moveDirection
, followTokenList
, getPositionValue
, solve
, allPossibleCordsForDiamond
) where

import Control.Monad
import Data.List
import Data.Char

-- Tokenizer

data Token a = Move a | Next deriving (Eq, Show)

tokenize :: String -> [Token String]
tokenize [] = []
tokenize (x:xs)
  | elem x "ULDR" = Move [x]:tokenize xs
  | x == '\n' = next
  | otherwise = tokenize xs
    where next = if (length xs /= 0 && '\n' == head xs )
                  then tokenize xs
                  else Next:tokenize xs


-- Follow tokenlist

type Pos = (Int, Int)

square3by3bounds (x, y) = all (\x' -> x' >= 0 && x' <= 2) [x, y]

allPossibleCordsForDiamond = do
  x <- [0..4]
  y <- [0..4]

  let cord = (x, y)

  let minY = 2 - (mod x 2);
  let maxY = 2 + (mod x 2);

  guard $ (elem y [minY..maxY] || x == 2)

  return cord

diamondBounds pos = elem pos allPossibleCordsForDiamond

moveDirection :: String -> Pos -> Pos
moveDirection direction (x, y) = if (inBounds newPosition) then newPosition else (x, y)
  where inBounds = diamondBounds -- square3by3bounds
        newPosition
          | direction == "U" = (x - 1, y)
          | direction == "D" = (x + 1, y)
          | direction == "R" = (x, y + 1)
          | direction == "L" = (x, y - 1)

followTokenList :: Pos -> [Token String] -> [Pos]
followTokenList pos [] = [pos]
followTokenList pos (tok:toks) = case tok of
    Move dir -> followTokenList (moveDirection dir pos) toks
    Next -> if (length toks == 0)
               then followTokenList pos toks
               else pos:followTokenList pos toks

-- calculate position values

getPositionValue pos = case elemIndex pos allPossibleCordsForDiamond of
                         Just x -> toUpper $ intToDigit (x + 1)
                         Nothing -> '0'


-- Solve it

solve :: String -> String
solve x = fmap (getPositionValue) . followTokenList (2, 0) $ tokenize x


