import Test.HUnit
import Lib

-- Geometry stuff

testValidTriangle =
  test ["testInvalidTriangle" ~: "One side is larger, should be false"
        ~: True ~=? all (== False) (fmap isValidTriangle [(5,10,25), (10, 5, 25), (25, 10, 5)])
       ,"testValidTriangle" ~: "Should be valid"
        ~: True ~=? isValidTriangle (5,5,5)
       ,"testListToTriangle" ~: "Conversion"
        ~: (1,12,3) ~=? listToTriangle ["1","12","3"]
       ,"testListToTriangle" ~: "Conversion"
        ~: (1,12,33) ~=? listToTriangle ["1","12","33"]
       ]

-- Parser

testParser =
  test ["testParser" ~: "Parse a string of data separated by spaces and newlines"
        ~: 3 ~=? length (parse "123 123 123 \n 321 22 1 \n 123 11 1")
       ,"testParser2" ~: "Parse a string of data separated by spaces and newlines"
        ~: (1, 12, 123) ~=? head (parse "1 12 123 \n 321 22 1 \n 123 11 1")
       ]

testColumnParser =
  test ["testColumnParser" ~: "Parse a string of data separated by spaces and newlines"
        ~: 3 ~=? length (columnParser "123 123 123 \n 321 22 1 \n 123 11 1")
       ,"testColumnParser2" ~: "Parse a string of data separated by spaces and newlines"
        ~: [(1, 321, 123), (12, 22, 11), (123, 1, 1)] ~=? columnParser "1 12 123 \n 321 22 1 \n 123 11 1"
       ]


main :: IO ()
main = do
  runTestTT testValidTriangle
  runTestTT testParser
  runTestTT testColumnParser
  return ()

