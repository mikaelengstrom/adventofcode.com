module Main where

import Data.List
import Data.Char

main :: IO ()
main = putStrLn "hej"

test :: Int -> Int
test x = x * 2

data Direction = North | East | South | West deriving (Show, Eq)
data LeftOrRight = L | R deriving (Show)
type Position = (Int, Int)
type Distance = Int

directions = [North, East, South, West]
getDirectionIndex :: Direction -> Int
getDirectionIndex x = case elemIndex x directions of Just y -> y

charToLeftOrRight :: Char -> LeftOrRight
charToLeftOrRight x
  | x == 'L' = L
  | x == 'R' = R

stringToInt :: String -> Int
stringToInt x = read x :: Int

turn :: Direction -> LeftOrRight -> Direction
turn dir lor = directions!!newDirectionIndex
  where newDirectionIndex = case lor of
                              R -> (oldDirectionIndex + 1) `mod` (length directions)
                              L -> (oldDirectionIndex + (length directions) - 1) `mod` (length directions)
        oldDirectionIndex = getDirectionIndex dir


tuppleMove :: (Position, Direction) -> (Position, Direction)
tuppleMove ((x, y), North) = ((x + 1, y), North)
tuppleMove ((x, y), East) = ((x, y + 1), East)
tuppleMove ((x, y), South) = ((x - 1, y), South)
tuppleMove ((x, y), West) = ((x, y - 1), West)


move :: (Position, Direction) -> Distance -> [Position]
move (pos, dir) distance = if (distance > 0)
                               then pos:(move (tuppleMove (pos, dir)) (distance - 1))
                               else [pos]


directionMap = "L5, R1, L5, L1, R5, R1, R1, L4, L1, L3, R2, R4, L4, L1, L1, R2, R4, R3, L1, R4, L4, L5, L4, R4, L5, R1, R5, L2, R1, R3, L2, L4, L4, R1, L192, R5, R1, R4, L5, L4, R5, L1, L1, R48, R5, R5, L2, R4, R4, R1, R3, L1, L4, L5, R1, L4, L2, L5, R5, L2, R74, R4, L1, R188, R5, L4, L2, R5, R2, L4, R4, R3, R3, R2, R1, L3, L2, L5, L5, L2, L1, R1, R5, R4, L3, R5, L1, L3, R4, L1, L3, L2, R1, R3, R2, R5, L3, L1, L1, R5, L4, L5, R5, R2, L5, R2, L1, L5, L3, L5, L5, L1, R1, L4, L3, L1, R2, R5, L1, L3, R4, R5, L4, L1, R5, L1, R5, R5, R5, R2, R1, R2, L5, L5, L5, R4, L5, L4, L4, R5, L2, R1, R5, L1, L5, R4, L3, R4, L2, R3, R3, R3, L2, L2, L2, L1, L4, R3, L4, L2, R2, R5, L1, R2"

data Token a = Turn a | Move a | Pass deriving (Show, Eq)

tokenize :: String -> [Token String]
tokenize [] = []
tokenize (x:xs)
  | isDigit x = number [] (x:xs)
  | elem x "LR" = (Turn [x]):(tokenize xs)
  | otherwise = Pass:(tokenize xs)
    where
      number stack [] = [Move $ reverse stack]
      number stack (x':xs') = if (isDigit x')
                                   then number (x':stack) xs'
                                   else (Move $ reverse stack):(tokenize (x':xs'))


followMap :: Position -> Direction -> [Token String] -> [Position]
followMap pos _ [] = [pos]
followMap pos dir (token:tokenList) = case token of
                                        Move distance -> let positions = move (pos, dir) (stringToInt distance)
                                                          in positions ++ (tail $ followMap (last $ positions) dir tokenList)
                                        Turn lor -> followMap pos (turn dir (charToLeftOrRight $ head lor)) tokenList
                                        Pass -> followMap pos dir tokenList

sumCords (x, y) = toPositive x + toPositive y
  where toPositive x = if (x > 0) then x else x * (-1)

solutionStep1 = sumCords $ last $ followMap (0,0) North $ tokenize directionMap

firstLocationVisitedTwice :: [Position] -> Position
firstLocationVisitedTwice [] = (0, 0)
firstLocationVisitedTwice (x:xs) = if (elem x xs) then x else firstLocationVisitedTwice xs

solutionStep2 = sumCords $ firstLocationVisitedTwice positions
  where
    positions = followMap (0,0) North $ tokenize directionMap
