import Lib
import Test.HUnit

-- Tokenizer

tokTests = test ["len" ~: "Tokenizer return the wrong length" ~: 5 ~=? length (tokenize "ULDR\n")
                ,"len2" ~: "Tokenizer the wrong length" ~: 4 ~=? length (tokenize "ULDRasdf ")
                ,"main" ~: "Tokenizer returned the wrong result"
                   ~: [Move "U", Move "D", Move "L", Move "R"] ~=? tokenize "UDLR"
                ,"nls" ~: "Tokenizer did not tokenize newlines"
                   ~: True ~=? elem Next (tokenize "ULL\nRRDDD\nLURDL\nUUUUD")
                ,"nls2" ~: "Tokenizer did catch multiple newlines"
                   ~: 4 ~=? length (filter (==Next) (tokenize "ULL\n\n\nRRDDD\n\n\nLURDL\nUUUUD\n\n"))
                ]


-- Follow tokenlist

tokenList = [Move "U", Move "R", Next, Move "D", Next]

followTests = test ["len" ~: "followTokenList returned wrong list length"
                    ~: 2 ~=? length (followTokenList (0, 0) tokenList)

                   ,"moveUp" ~: "Move Up did not move"
                    ~: (1, 2) ~=? moveDirection "U" (2, 2)

                   ,"moveUpOut" ~: "Move Up out of bounds moved"
                    ~: (0, 2) ~=? moveDirection "U" (0, 2)

                   ,"moveDown" ~: "Move Down did not move"
                    ~: (3, 2) ~=? moveDirection "D" (2, 2)

                   ,"moveDownOut" ~: "Move Down out of bounds moved"
                    ~: (4, 2) ~=? moveDirection "D" (4, 2)

                   ,"moveRight" ~: "Move Right did not move"
                   ~: (2, 3) ~=? moveDirection "R" (2, 2)

                   ,"moveRightOut" ~: "Move right out of bounds moved"
                    ~: (2, 4) ~=? moveDirection "R" (2, 4)

                   ,"moveLeft" ~: "Move Left did not move"
                   ~: (2, 1) ~=? moveDirection "L" (2, 2)

                   ,"moveLeftOut" ~: "Move left out of bounds moved"
                    ~: (2, 0) ~=? moveDirection "L" (2, 0)

                   ,"movingAround" ~: "Moving around a bit"
                      ~: [(2, 1)] ~=? followTokenList (2,2) [Move "L", Move "U", Move "R", Move "D", Move "L"]
                   ]


-- calculate position values

calcPosTests =
  test ["calcPosition" ~: "Calculation failed"
          ~: "123456789ABCD" ~=? fmap getPositionValue allPossibleCordsForDiamond]

-- Solve it

solveTest =
  test ["solveTest" ~: "Could not solve"
        ~: "5DB3" ~=? solve "ULL\nRRDDD\nLURDL\nUUUUD"]

-- Read stdin

main :: IO ()
main = do
  runTestTT tokTests
  runTestTT followTests
  runTestTT calcPosTests
  runTestTT solveTest
  return ()
