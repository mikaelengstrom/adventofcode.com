import Lib
import Test.HUnit

-- Tokenizer

tokTests = test ["len" ~: "Tokenizer return the wrong length" ~: 4 ~=? length (tokenize "ULDR")
                ,"len2" ~: "Tokenizer the wrong length" ~: 4 ~=? length (tokenize "ULDRasdf ")
                ,"main" ~: "Tokenizer returned the wrong result"
                   ~: [Move "U", Move "D", Move "L", Move "R"] ~=? tokenize "UDLR"
                ,"nls" ~: "Tokenizer did not tokenize newlines"
                   ~: True ~=? elem Next (tokenize "ULL\nRRDDD\nLURDL\nUUUUD")
                ]

-- Follow tokenlist

tokenList = [Move "U", Move "R", Next, Move "D"]

followTests = test ["len" ~: "followTokenList returned wrong list length"
                      ~: 2 ~=? length (followTokenList (0, 0) tokenList)
                   ,"moveUp" ~: "Move Up did not move"
                      ~: (1, 0) ~=? moveDirection "U" (1, 1)
                   ,"moveUpOut" ~: "Move Up out of bounds moved"
                      ~: (0, 0) ~=? moveDirection "U" (0, 0)
                   ,"moveDown" ~: "Move Down did not move"
                      ~: (1, 2) ~=? moveDirection "D" (1, 1)
                   ,"moveDownOut" ~: "Move Down out of bounds moved"
                      ~: (2, 2) ~=? moveDirection "D" (2, 2)
                   ,"moveRight" ~: "Move Right did not move"
                      ~: (2, 1) ~=? moveDirection "R" (1, 1)
                   ,"moveRightOut" ~: "Move right out of bounds moved"
                      ~: (2, 2) ~=? moveDirection "R" (2, 2)
                   ,"moveLeft" ~: "Move Left did not move"
                      ~: (0, 1) ~=? moveDirection "L" (1, 1)
                   ,"moveLeftOut" ~: "Move left out of bounds moved"
                      ~: (0, 0) ~=? moveDirection "L" (0, 0)
                   ,"movingAround" ~: "Moving around a bit"
                      ~: [(1, 2)] ~=? followTokenList (2,2) [Move "L", Move "U", Move "R", Move "D", Move "L"]
                   ,"equals" ~: "Token list returned the wrong positions"
                      ~: [(2, 0), (2, 1)] ~=? followTokenList (1,1) tokenList
                   ,"equals" ~: "Token list did move out of bounds"
                      ~: [(2, 0), (2, 1)] ~=? followTokenList (2,0) tokenList]


-- calculate position values

allPos = [(x, y) | y <- [0..2], x <- [0..2]]
calcPosTests =
  test ["calcPosition" ~: "Calculation failed"
          ~: [1..9] ~=? fmap getPositionValue allPos]

-- Solve it

solve :: String -> String
solve x = foldl (++) "" $ fmap (show . getPositionValue) . followTokenList (1,1) $ tokenize x

solveTest =
  test ["solveTest" ~: "Could not solve"
        ~: "1985" ~=? solve "ULL\nRRDDD\nLURDL\nUUUUD"]

-- Read stdin

main :: IO ()
main = do
  runTestTT tokTests
  runTestTT followTests
  runTestTT calcPosTests
  runTestTT solveTest
  return ()
