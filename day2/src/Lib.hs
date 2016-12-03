module Lib
( tokenize
, Token(Move, Next)
, Pos
, moveDirection
, followTokenList
, getPositionValue
) where

-- Tokenizer

data Token a = Move a | Next deriving (Eq, Show)

tokenize :: String -> [Token String]
tokenize [] = []
tokenize (x:xs)
  | elem x "ULDR" = Move [x]:tokenize xs
  | x == '\n' = Next:tokenize xs
  | otherwise = tokenize xs


-- Follow tokenlist

type Pos = (Int, Int)

moveDirection :: String -> Pos -> Pos
moveDirection direction (x, y) = if (inBounds newPosition) then newPosition else (x, y)
  where inBounds (x, y) = all (\x' -> x' >= 0 && x' <= 2) [x, y]
        newPosition
          | direction == "U" = (x, y - 1)
          | direction == "D" = (x, y + 1)
          | direction == "R" = (x + 1, y)
          | direction == "L" = (x - 1, y)

followTokenList :: Pos -> [Token String] -> [Pos]
followTokenList pos [] = [pos]
followTokenList pos (tok:toks) = case tok of
    Move dir -> followTokenList (moveDirection dir pos) toks
    Next -> pos:followTokenList pos toks

-- calculate position values

getPositionValue (x, y) = y * 3 + x + 1


